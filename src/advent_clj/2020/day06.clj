(ns advent-clj.2020.day06
  (:require [advent-clj.core :refer [get-partitioned-input]]
            [clojure.set :refer [intersection union]]))

(def input (get-partitioned-input "2020/day06.txt"))

(def into-set (comp #(into #{} %) seq))

(defn solve-by [f input]
  (let [solver (comp count #(apply f %) #(map into-set %))]
    (->> input (map solver) (reduce +))))

(defn -main []
  (println "Advent of Code 2020-06.1:" (solve-by union input))
  (println "Advent of Code 2020-06.2:" (solve-by intersection input)))
