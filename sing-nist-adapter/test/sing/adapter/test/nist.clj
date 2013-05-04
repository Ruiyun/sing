(ns sing.adapter.test.nist
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :refer [upper-case]]
            [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [sing.adapter.nist :refer :all])
  (:import [java.util Properties]
           [javax.sip SipProvider ListeningPoint]
           [gov.nist.javax.sip SipStackImpl]))

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(with-private-fns [sing.adapter.nist [kw->propname create-properties create-provider]]
  (deftest test-keyword->property-name
    (testing "normal JAIN property."
      (is (= (kw->propname :property-name) "javax.sip.PROPERTY_NAME")))
    (testing "normal nist property."
      (is (= (kw->propname :nist-property-name) "gov.nist.javax.sip.PROPERTY_NAME")))
    (testing "have ':nist-' only."
      (is (nil? (kw->propname :nist-))))
    (testing "do not match ':nist-' completly."
      (are [x y] (= (kw->propname x) y)
           :nist "javax.sip.NIST"
           :nistproperty-name "javax.sip.NISTPROPERTY_NAME")))
  (deftest test-create-properties
    (testing "type of return value."
      (is (instance? Properties (create-properties {:name "sing-test"}))))
    (testing "options have name and others."
      (is (= (create-properties {:name "sing-test", :prop-1 "prop 1", :nist-prop-2 "prop 2"})
             (doto (Properties.)
               (.setProperty "javax.sip.STACK_NAME" "sing-test")
               (.setProperty "javax.sip.PROP_1" "prop 1")
               (.setProperty "gov.nist.javax.sip.PROP_2" "prop 2")))))
    (testing "options do not have name."
      (is (thrown? AssertionError (create-properties {:prop-1 "prop 1", :nist-prop-2 "prop 2"}))))
    (testing "values contains nil."
      (are [x] (thrown? AssertionError (create-properties x))
           {:name nil} {:name "sing-test", :prop nil}))
    (testing "numeric value auto convert to string."
      (are [x y] (= (create-properties {:name "sing-test", :prop x})
                    (doto (Properties.)
                      (.setProperty "javax.sip.STACK_NAME" "sing-test")
                      (.setProperty "javax.sip.PROP" y)))
           12 "12"
           123.45 "123.45")))
  (deftest test-create-provider
    (testing "SipProvider without tcp nor udp."
      (is (thrown? AssertionError (create-provider {:name "sing-test" :host "127.0.0.1" :port 5060}))))
    (testing "SipProvider with 5060 udp."
      (let [^SipProvider p (create-provider {:name "sing-test", :host "127.0.0.1", :port 5060, :udp? true})
            eps (.getListeningPoints p)]
        (is (= (alength eps) 1))
        (let [^ListeningPoint ep (aget eps 0)]
          (is (= (upper-case (.getTransport ep)) ListeningPoint/UDP))
          (is (= (.getPort ep) 5060))
          (is (= (.getIPAddress ep) "127.0.0.1")))
        (.. p (getSipStack) (stop))))
    (testing "SipProvider with 5070 tcp."
      (let [^SipProvider p (create-provider {:name "sing-test", :host "127.0.0.1", :port 5070, :tcp? true})
            eps (.getListeningPoints p)]
        (is (= (alength eps) 1))
        (let [^ListeningPoint ep (aget eps 0)]
          (is (= (upper-case (.getTransport ep)) ListeningPoint/TCP))
          (is (= (.getPort ep) 5070))
          (is (= (.getIPAddress ep) "127.0.0.1")))
        (.. p (getSipStack) (stop))))
    (testing "SipProvider with 5060 udp & tcp."
      (let [^SipProvider p (create-provider {:name "sing-test", :host "127.0.0.1", :port 5060, :udp? true, :tcp? true})]
        (is (= (alength (.getListeningPoints p)) 2))
        (let [tcp (.getListeningPoint p ListeningPoint/TCP)
              udp (.getListeningPoint p ListeningPoint/UDP)]
          (is (not (or (nil? tcp) (nil? udp)))))
        (.. p (getSipStack) (stop))))))

(deftest scenario-test
  (testing "invite with simple text"
    (let [alice-received (atom [])
          bob-received (atom [])
          alice-handler (fn [{method :method :as request}]
                          (swap! alice-received conj request)
                          {:status 200})
          bob-handler (fn [{method :method :as request}]
                        (swap! bob-received conj request)
                        (case method
                          :invite (let [final-response (promise)]
                                    (future
                                      (Thread/sleep 2000)
                                      (deliver final-response {:status 200, :content-type "application/sdp" :content "o=..."}))
                                    {:status 180, :modify {:to-tag "a6c85cf"} :next final-response})
                          :ack nil))
          {alice-close :close, alice-send-request :send-request} (run-nist alice-handler {:port 5060})
          {bob-close :close, bob-send-request :send-request} (run-nist bob-handler {:port 5070})]
      (-> (alice-send-request {:method :invite
                               :uri "sip:bob@127.0.0.1:5070"
                               :from "Alice <sip:alice@127.0.0.1:5060>;tag=1928301774"
                               :to "Bob <sip:bob@127.0.0.1:5070>"
                               :call-id "a84b4c76e66710"
                               :cseq 314159
                               :max-forwards 70
                               :headers {"contact" "<sip:alice@pc33.atlanta.com>"}
                               :content-type "application/sdp"
                               :content "c=..."})
          deref
          (as-> response
                (are [x y] (= x y)
                     (:status response) 180
                     (:to response) "Bob <sip:bob@127.0.0.1:5070>;tag=a6c85cf")
                (:next response))
          deref
          (as-> response
                (are [x y] (= x y)
                     (:status response) 200
                     (:to response) "Bob <sip:bob@127.0.0.1:5070>;tag=a6c85cf")
                (select-keys response [:call-id :from :to]))
          (as-> dialog-identifier
                (-> (merge {:method :ack
                            :uri "sip:bob@127.0.0.1:5070"
                            :cseq 2}
                           dialog-identifier)
                    alice-send-request
                    deref
                    (as-> response
                          (is (= response :not-applicable))))
                dialog-identifier)
          (as-> dialog-identifier
                (bob-send-request {}))))))
