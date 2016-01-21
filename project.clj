(defproject blx.sortable "0.1.0-SNAPSHOT"
  :description "Solution to the Sortable coding challenge. Attempts to match product listings with a known set of products."
  :url "https://github.com/blx/sortable-challenge"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [cheshire "5.5.0"]
                 [tesser.core "1.0.1"]]
  :main ^:skip-aot blx.sortable.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[com.taoensso/timbre "4.2.1"]]}})
