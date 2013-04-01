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
