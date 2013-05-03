(defproject sing "0.1.0-SNAPSHOT"
  :description "A ring like Clojure SIP library."
  :url "https://github.com/Ruiyun/sing"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [sing/sing-nist-adapter "0.1.0-SNAPSHOT"]]
  :plugins [[lein-sub "0.2.4"]
            [codox "0.6.4"]]
  :sub ["sing-nist-adapter"]
  :codox {:sources ["sing-nist-adapter/src"]})
