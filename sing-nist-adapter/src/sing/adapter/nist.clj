(ns sing.adapter.nist
  "Adapter for the nist implemention of JAIN.

   It provider a ring like api for SIP.
   A typical completed request is looks like:
     {:method         :invite
      :uri            \"sip:bob@biloxi.com\"
      :local-addr     \"192.0.2.1\"
      :local-port     \"5060\"
      :remote-addr    \"192.0.2.2\"
      :remote-port    \"5060\"
      :via            [\"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8\"]
      :call-id        \"a84b4c76e66710\"
      :cseq           314159
      :from           \"Alice <sip:alice@atlanta.com>;tag=1928301774\"
      :to             \"Bob <sip:bob@biloxi.com>\"
      :max-forwards   70
      :headers        {\"via\"            \"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8\"
                       \"call-id\"        \"a84b4c76e66710\"
                       \"cseq\"           \"314159 INVITE\"
                       \"from\"           \"Alice <sip:alice@atlanta.com>;tag=1928301774\"
                       \"to\"             \"Bob <sip:bob@biloxi.com>\"
                       \"max-forwards\"   \"70\"
                       \"contact\"        \"<sip:alice@pc33.atlanta.com>\"
                       \"content-type\"   \"application/sdp\"
                       \"content-length\" \"142\"}
      :content-type   \"application/sdp\"
      :content-length 142
      :content        ... (Alice's SDP not shown)}
   A typical completed response is looks like:
     {:status         200
      :reason         \"OK\"
      :final?         true
      :local-addr     \"192.0.2.4\"
      :local-port     \"5060\"
      :remote-addr    \"192.0.2.3\"
      :remote-port    \"5060\"
      :via            \"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8;received=192.0.2.1\"
      :call-id        \"a84b4c76e66710\"
      :cseq           314159
      :from           \"Alice <sip:alice@atlanta.com>;tag=1928301774\"
      :to             \"Bob <sip:bob@biloxi.com>;tag=a6c85cf\"
      :headers        {\"via\" \"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8;received=192.0.2.1\"
                       \"to\" \"Bob <sip:bob@biloxi.com>;tag=a6c85cf\"
                       \"from\" \"Alice <sip:alice@atlanta.com>;tag=1928301774\"
                       \"call-id\" \"a84b4c76e66710\"
                       \"cseq\" \"314159 INVITE\"
                       \"contact\" \"<sip:bob@192.0.2.4>\"
                       \"content-type\" \"application/sdp\"
                       \"content-length\" \"131\"}
      :content-type   \"application/sdp\"
      :content-length 131
      :content        ... (Bob's SDP not shown)}"
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :refer [upper-case lower-case replace join]]
            [sing.adapter.nist.core :refer :all])
  (:import [java.util Properties]
           [javax.sip SipFactory SipStack SipProvider SipListener ListeningPoint Timeout ClientTransaction]
           [gov.nist.javax.sip.message SIPResponse]))

(defonce ^{:private true
           :doc "The instance of JAIN-SIP's Factories."}
  factories (let [f (doto (SipFactory/getInstance) (.setPathName "gov.nist"))]
              {:sip f
               :address (.createAddressFactory f)
               :message (.createMessageFactory f)
               :header (.createHeaderFactory f)}))

(defn- fill-prefix [^String name]
  (if (.startsWith name "NIST_")
    (if-let [n (seq (subs name 5))]
      (->> (apply str n) (str "gov.nist.javax.sip."))
      nil)
    (str "javax.sip." name)))

(defn- kw->propname [kw]
  (-> kw name (replace #"-" "_") upper-case fill-prefix))

(defn- create-properties [options]
  {:pre [(contains? options :name)
         (not-any? nil? (vals options))]}
  (let [ops (-> options (assoc :stack-name (:name options)) (dissoc :name))
        p (Properties.)]
    (doseq [o ops]
      (.setProperty p (-> o first kw->propname) (str (second o))))
    p))

(defn- ^SipProvider create-provider
  [{:keys [host port tcp? udp?] :as options}]
  {:pre [(or tcp? udp?)]}
  (let [s (.createSipStack ^SipFactory (:sip factories)
                           (-> (dissoc options :host :port :tcp? :udp?)
                               create-properties))
        udp (.createListeningPoint s host port ListeningPoint/UDP)
        p (.createSipProvider s udp)]
    (when tcp? (.addListeningPoint p (.createListeningPoint s host port ListeningPoint/TCP)))
    (when (not udp?) (.removeListeningPoint p udp))
    p))

(defn- deliver-response [event response]
  (-> (.. event getClientTransaction getApplicationData)
      (deliver response)))

(defn- proxy-handler
  [handler]
  (reify SipListener
    (processRequest [this event])
    (processResponse [this event]
      (let [rsp ^SIPResponse (.getResponse event)]
        (when (.isFinalResponse rsp)
          (->> (build-response-map rsp)
               (deliver-response event)))))
    (processTimeout [this event]
      (when-not (.isServerTransaction event)
        (if (= (.getTimeout event) Timeout/TRANSACTION)
          (deliver-response event :transaction-timeout)
          (deliver-response event :retransmit-timeout))))
    (processTransactionTerminated [this event])
    (processDialogTerminated [this event])
    (processIOException [this event])))

(defn- send-request [request-map, ^SipProvider sip-provider]
  (let [t (->> (build-nist-request request-map)
               (.getNewClientTransaction sip-provider))
        rsp (promise)]
    (doto t
      (.setApplicationData rsp)
      (.sendRequest))
    rsp))

(defn ^SipProvider run-nist
  "Start a nist SIP stack to serve the given handler according
  to the supplied options:

  :configurator - a function called with the SipProvider instance
  :host         - the hostname to listen on (default to 0.0.0.0)
  :port         - the port to listen on (default to 5060)
  :name         - the name of stack (default to sing-version)
  :tcp?         - use TCP transport (default to false)
  :udp?         - use UDP transport (default to true)

  The 'handler' argument is a function that take one parameter. When nist received a SIP request
  from peer. It will call the handler function with a completed request map. And the handler
  function must return a response map or nil if the request is an ACK. Then the response map will
  send to peer by nist.

  the function return a map contains two functions.
    :close        it take no parameter, call it to close the nist.
    :send-request it take one request map, call it to send a sip request.
                  A sip request map argument is looks like
                    {:method         :invite
                     :uri            \"sip:bob@biloxi.com\"
                     :via            [\"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8\"]
                     :call-id        \"a84b4c76e66710\"
                     :cseq           314159
                     :from           \"Alice <sip:alice@atlanta.com>;tag=1928301774\"
                     :to             \"Bob <sip:bob@biloxi.com>\"
                     :max-forwards   70
                     :content-type   \"application/sdp\"
                     :content        ... (Alice's SDP not shown, could be a byte array, a string or others)}

                  This function will return a 'promise' immediately. when nist receive the response from UAS,
                  the promise will be realized to a response map. Otherwise, it will be realized to a keyword
                  that indicate the reason for failure."
  [handler options]
  (let [p (-> {:host "0.0.0.0", :port 5060, :name "sing-0.1.0", :tcp? false, :udp? true}
              (merge options)
              (dissoc :stack-name :configurator)
              (assoc :automatic-dialog-support "OFF")
              create-provider)]
    (.addSipListener p (proxy-handler handler))
    (when-let [c (:configurator options)] (c p))
    (.. p getSipStack start) (.start (.getSipStack p))
    {:close #(.. p getSipStack stop)
     :send-request #(send-request % p)}))
