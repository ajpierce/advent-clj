(defproject advent-clj "0.1.0-SNAPSHOT"
  :description "Advent of Code Solutions, just for fun"
  :url "http://github.com/ajpierce/advent-clj"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :main ^:skip-aot advent-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
