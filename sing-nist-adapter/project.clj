(defproject sing/sing-nist-adapter "0.1.0-SNAPSHOT"
  :description "Sing jain-nist adapter."
  :url "https://github.com/Ruiyun/sing"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[javax.sip/jain-sip-api "1.2.1.4"]
                 [javax.sip/jain-sip-ri "1.2.166"]]
  :lein-release {:deploy-via :clojars}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.5.1"]
                                  [log4j/log4j "1.2.16"]]}
             :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}})
