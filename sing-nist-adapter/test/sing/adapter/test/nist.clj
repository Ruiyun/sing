(ns sing.adapter.test.nist
  (:use clojure.test
        sing.adapter.nist))

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(with-private-fns [sing.adapter.nist [kw->propname create-properties]]
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
      (is (instance? java.util.Properties (create-properties {:name "ring-test"}))))
    (testing "options have name and others."
      (is (= (create-properties {:name "ring-test", :prop-1 "prop 1", :nist-prop-2 "prop 2"})
             (doto (java.util.Properties.)
               (.setProperty "javax.sip.STACK_NAME" "ring-test")
               (.setProperty "javax.sip.PROP_1" "prop 1")
               (.setProperty "gov.nist.javax.sip.PROP_2" "prop 2")))))
    (testing "options do not have name."
      (is (thrown? AssertionError (create-properties {:prop-1 "prop 1", :nist-prop-2 "prop 2"}))))
    (testing "values contains nil."
      (are [x] (thrown? AssertionError (create-properties x))
           {:name nil} {:name "ring-test", :prop nil}))))
