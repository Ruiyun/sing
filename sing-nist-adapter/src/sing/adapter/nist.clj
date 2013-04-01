(ns sing.adapter.nist
  "Adapter for the nist implemention of JAIN."
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :refer [upper-case lower-case replace join]])
  (:import [java.util Properties]
           [javax.sip SipFactory SipStack SipProvider SipListener ListeningPoint]
           [javax.sip.address AddressFactory]
           [javax.sip.message MessageFactory Request Response]
           [javax.sip.header HeaderFactory ContentTypeHeader]
           [gov.nist.javax.sip.message SIPRequest]
           [gov.nist.javax.sip.header SIPHeader]))

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

(defn- get-headers [^Request request]
  (reduce
   (fn [headers header-name]
     (assoc headers
       (-> (lower-case header-name) keyword)
       (->> (.getHeaders request header-name)
            enumeration-seq
            (map #(.getValue ^SIPHeader %)) ; urgly, (memfn getValue) is more better, but fire reflection warning
            (join ", "))))
   {}
   (enumeration-seq (.getHeaderNames request))))

(defn build-request-map [^SIPRequest request]
  {:local-addr          (when-let [a (.getLocalAddress request)] (.getHostAddress a))
   :local-port          (.getLocalPort request)
   :remote-addr         (when-let [a (.getRemoteAddress request)] (.getHostAddress a))
   :remote-port         (.getRemotePort request)
   :method              (.getMethod request)
   :uri                 (.getRequestURI request)
   :headers             (get-headers request)
   :expires             (when-let [h (.getExpires request)] (.getExpires h))
   :content-type        (when-let [^ContentTypeHeader h (.getHeader request ContentTypeHeader/NAME)]
                          {:type (.getContentType h), :sub-type (.getContentSubType h)})
   :content-length      (when-let [h (.getContentLength request)] (.getContentLength h))
   :content-encoding    (when-let [h (.getContentEncoding request)] (.getEncoding h))
   :content-language    (when-let [h (.getContentLanguage request)] (str (.getContentLanguage h)))
   :content-disposition (when-let [h (.getContentDisposition request)]
                          {:type (.getDispositionType h), :handling (.getHandling h)})
   :content             (.getRawContent request)})

(defn- proxy-handler
  [handler]
  (reify SipListener
    (processRequest [this event])))

(defn ^SipProvider run-nist
  "Start a nist SIP stack to serve the given handler according
  to the supplied options:

  :configurator - a function called with the SipProvider instance
  :host         - the hostname to listen on (default to 0.0.0.0)
  :port         - the port to listen on (default to 5060)
  :name         - the name of stack (default to sing-version)
  :tcp?         - use TCP transport (default to false)
  :udp?         - use UDP transport (default to true)"
  [handler options]
  (let [p (-> {:host "0.0.0.0", :port 5060, :name "sing-0.1.0", :tcp? false, :udp? true}
              (merge options)
              (dissoc :stack-name :configurator)
              create-provider)]
    (.addSipListener p (proxy-handler handler))
    (when-let [c (:configurator options)] (c p))
    (.start (.getSipStack p))
    p))
