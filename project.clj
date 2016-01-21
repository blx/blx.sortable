(defproject blx.sortable "0.1.0"
  :description "Solution to the Sortable coding challenge. Matches product listings with a known set of products."
  :url "https://github.com/blx/sortable-challenge"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.cli "0.3.3"]
                 [cheshire "5.5.0"]
                 [tesser.core "1.0.1"]
                 [com.taoensso/timbre "4.2.1"]
                 [prismatic/plumbing "0.5.2"]]
  :main ^:skip-aot blx.sortable.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
