(defproject cloker "0.1.0-SNAPSHOT"
  :description "Cloker is a rudimentary poker engine written in Clojure."
  :url "https://github.com/will2dye4/cloker"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :main ^:skip-aot cloker.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
