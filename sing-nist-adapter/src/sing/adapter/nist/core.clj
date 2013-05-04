(ns sing.adapter.nist.core
  (:require [clojure.string :refer [lower-case upper-case join]])
  (:import [javax.sip.message MessageFactory]
           [javax.sip.header HeaderFactory]
           [gov.nist.javax.sip.message MessageFactoryImpl SIPMessage SIPRequest SIPResponse]
           [gov.nist.javax.sip.header HeaderFactoryImpl SIPHeader CallID CSeq From To Via MaxForwards]
           [gov.nist.javax.sip.parser StringMsgParser]))

(defn- get-headers [^SIPMessage request]
  (reduce
   (fn [headers header-name]
     (assoc headers
       (lower-case header-name)
       (->> (.getHeaders request header-name)
            iterator-seq
            (map (memfn getValue))
            (join ", "))))
   {}
   (iterator-seq (.getHeaderNames request))))

(defn- build-message-map [^SIPMessage message]
  {:local-addr          (when-let [addr (.getLocalAddress message)] (.getHostAddress addr))
   :local-port          (.getLocalPort message)
   :remote-addr         (when-let [addr (.getRemoteAddress message)] (.getHostAddress addr))
   :remote-port         (.getRemotePort message)
   :headers             (get-headers message)
   :call-id             (.. message getCallId getCallId)
   :cseq                (.. message getCSeq getSeqNumber)
   :from                (let [^From h (.getFrom message)] (.getValue h))
   :to                  (let [^To h (.getTo message)] (.getValue h))
   :via                 (mapv #(.getValue ^Via %) (seq (.getViaHeaders message)))
   :content-type        (when-let [h (.getContentTypeHeader message)] (.getValue h))
   :content-length      (when-let [h (.getContentLength message)] (.getContentLength h))
   :content             (.getRawContent message)})

(defn build-request-map [^SIPRequest request]
  (merge {:method       (-> (.getMethod request) lower-case keyword)
          :scheme       (-> (.. request getRequestURI getScheme) lower-case keyword)
          :uri          (.. request getRequestURI toString)
          :max-forwards (when-let [h (.getMaxForwards request)] (.getMaxForwards h))}
         (build-message-map request)))

(defn build-response-map [^SIPResponse response]
  (merge {:status (.getStatusCode response)
          :reason (.getReasonPhrase response)
          :final? (.isFinalResponse response)}
         (build-message-map response)))

(def ^:private ^MessageFactory message-factory (MessageFactoryImpl.))

(def ^:private ^HeaderFactory header-factory (HeaderFactoryImpl.))

(def ^:private ^StringMsgParser parser (StringMsgParser.))

(defn- set-headers! [^SIPMessage message, headers]
  (doseq [[key val-or-vals] (-> (reduce #(assoc %1 (-> %2 key name lower-case) (val %2)) {} headers)
                                (dissoc "call-id" "cseq" "from" "to" "via" "max-forwards" "content-type" "content-length"))]
    (if (string? val-or-vals)
      (.setHeader message (.createHeader header-factory key val-or-vals))
      (doseq [val val-or-vals]
        (.addHeader message (.createHeader header-factory key val)))))
  message)

(defn build-nist-request [{:keys [method uri call-id cseq from to via max-forwards content-type content headers]}]
  (let [method       (-> method name upper-case)
        uri          (.parseUrl parser uri)
        call-id      (.createCallIdHeader header-factory call-id)
        cseq         (.createCSeqHeader header-factory cseq method)
        from         (.createHeader header-factory "From" from)
        to           (.createHeader header-factory "To" to)
        via          (map #(.createHeader header-factory "Via" %) via)
        max-forwards (.createMaxForwardsHeader header-factory max-forwards)]
    (-> (if (and content-type content)
          (.createRequest message-factory uri method call-id cseq from to via max-forwards
                          (.createHeader header-factory "Content-Type" content-type) content)
          (.createRequest message-factory uri method call-id cseq from to via max-forwards))
        (set-headers! headers))))

(defn- set-to-tag-if-not-present! [^SIPResponse response, tag]
  (when-not (.hasToTag response)
    (.setToTag response (or tag (.. (Utils/getInstance) generateTag))))
  response)

(defn build-nist-response [nist-request {:keys [status to-tag content-type content headers]}]
  (-> (if (and content-type content)
        (.createResponse message-factory status nist-request (.createHeader header-factory "Content-Type" content-type) content)
        (.createResponse message-factory status nist-request))
      (set-headers! headers)
      (set-to-tag-if-not-present! to-tag)))
