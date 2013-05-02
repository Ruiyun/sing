(ns sing.adapter.nist.test.core-test
  (:require [clojure.test :refer :all]
            [sing.adapter.nist.core :refer :all])
  (:import [gov.nist.javax.sip.message SIPRequest]
           [gov.nist.javax.sip.parser StringMsgParser]
           [java.net InetAddress]))

(def parser (StringMsgParser.))

(deftest build-request-map-test
  (testing "basic register request"
    (is
     (= (build-request-map
         (doto (.parseSIPMessage
                ^StringMsgParser parser
                (.getBytes
                 (str
                  "REGISTER sip:nist.gov SIP/2.0\r\n"
                  "Via: SIP/2.0/UDP 129.6.55.182:14826\r\n"
                  "Max-Forwards: 70\r\n"
                  "From: <sip:mranga@nist.gov>;tag=6fcd5c7ace8b4a45acf0f0cd539b168b;epid=0d4c418ddf\r\n"
                  "To: <sip:mranga@nist.gov>\r\n"
                  "Call-ID: c5679907eb954a8da9f9dceb282d7230@129.6.55.182\r\n"
                  "CSeq: 1 REGISTER\r\n"
                  "Contact: <sip:129.6.55.182:14826>;methods=\"INVITE, MESSAGE, INFO, SUBSCRIBE, OPTIONS, BYE, CANCEL, NOTIFY, ACK, REFER\"\r\n"
                  "User-Agent: RTC/(Microsoft RTC)\r\n"
                  "Event:  registration\r\n"
                  "Allow-Events: presence\r\n"
                  "Content-Length: 0\r\n\r\n")) true false nil)
           (.setLocalAddress (InetAddress/getByName "129.6.55.181"))
           (.setLocalPort 5070)
           (.setRemoteAddress (InetAddress/getByName "129.6.55.182"))
           (.setRemotePort 5080)))
        {:method :register
         :scheme :sip
         :uri "sip:nist.gov"
         :local-addr "129.6.55.181"
         :local-port 5070
         :remote-addr "129.6.55.182"
         :remote-port 5080
         :headers {"via" "SIP/2.0/UDP 129.6.55.182:14826"
                   "max-forwards" "70"
                   "from" "<sip:mranga@nist.gov>;tag=6fcd5c7ace8b4a45acf0f0cd539b168b;epid=0d4c418ddf"
                   "to" "<sip:mranga@nist.gov>"
                   "call-id" "c5679907eb954a8da9f9dceb282d7230@129.6.55.182"
                   "cseq" "1 REGISTER"
                   "contact" "<sip:129.6.55.182:14826>;methods=\"INVITE, MESSAGE, INFO, SUBSCRIBE, OPTIONS, BYE, CANCEL, NOTIFY, ACK, REFER\""
                   "user-agent" "RTC/(Microsoft RTC)"
                   "event" "registration"
                   "allow-events" "presence"
                   "content-length" "0"}
         :call-id "c5679907eb954a8da9f9dceb282d7230@129.6.55.182"
         :cseq 1
         :from "<sip:mranga@nist.gov>;tag=6fcd5c7ace8b4a45acf0f0cd539b168b;epid=0d4c418ddf"
         :to "<sip:mranga@nist.gov>"
         :via ["SIP/2.0/UDP 129.6.55.182:14826"]
         :max-forwards 70
         :content-type nil
         :content-length 0
         :content nil})))
  (testing "security invite request"
    (let [sdp (str "v=0\r\n"
                   "o=4855 13760799956958020 13760799956958020 IN IP4  129.6.55.78\r\n"
                   "s=mysession session\r\n"
                   "p=+46 8 52018010\r\n"
                   "c=IN IP4  129.6.55.78\r\n"
                   "t=0 0\r\n"
                   "m=audio 6022 RTP/AVP 0 4 18\r\n"
                   "a=rtpmap:0 PCMU/8000\r\n"
                   "a=rtpmap:4 G723/8000\r\n"
                   "a=rtpmap:18 G729A/8000\r\n"
                   "a=ptime:20\r\n")
          actual (build-request-map
                  (.parseSIPMessage ^StringMsgParser parser
                                    (.getBytes
                                     (str "INVITE sips:littleguy@there.com:5060 SIP/2.0\r\n"
                                          "Via: SIP/2.0/UDP 65.243.118.100:5050\r\n"
                                          "Via: SIP/2.0/UDP 121.65.83.44:5068\r\n"
                                          "From: \"M. Ranganathan\" <sip:M.Ranganathan@sipbakeoff.com>;tag=1234\r\n"
                                          "To: \"littleguy@there.com\" <sip:littleguy@there.com:5060> \r\n"
                                          "Call-ID: Q2AboBsaGn9!?x6@sipbakeoff.com \r\n"
                                          "CSeq: 3 INVITE \r\n"
                                          "Content-Type: application/sdp\r\n"
                                          "Content-Length: 247\r\n\r\n"
                                          sdp) "UTF-8") true false nil))
          expects {:method :invite
                   :scheme :sips
                   :uri "sips:littleguy@there.com:5060"
                   :local-addr nil
                   :local-port 0
                   :remote-addr nil
                   :remote-port 0
                   :headers {"via" "SIP/2.0/UDP 65.243.118.100:5050, SIP/2.0/UDP 121.65.83.44:5068"
                             "from" "\"M. Ranganathan\" <sip:M.Ranganathan@sipbakeoff.com>;tag=1234"
                             "to" "\"littleguy@there.com\" <sip:littleguy@there.com:5060>"
                             "call-id" "Q2AboBsaGn9!?x6@sipbakeoff.com"
                             "cseq" "3 INVITE"
                             "content-type" "application/sdp"
                             "content-length" "247"}
                   :call-id "Q2AboBsaGn9!?x6@sipbakeoff.com"
                   :cseq 3
                   :from "\"M. Ranganathan\" <sip:M.Ranganathan@sipbakeoff.com>;tag=1234"
                   :to "\"littleguy@there.com\" <sip:littleguy@there.com:5060>"
                   :via ["SIP/2.0/UDP 65.243.118.100:5050" "SIP/2.0/UDP 121.65.83.44:5068"]
                   :max-forwards nil
                   :content-type "application/sdp"
                   :content-length 247
                   :content (.getBytes sdp)}]
      (is (= (dissoc actual :content) (dissoc expects :content)))
      (is (java.util.Arrays/equals (:content actual) (:content expects))))))
