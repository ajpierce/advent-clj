(ns advent-clj.2020.day06
  (:require [advent-clj.core :refer [get-partitioned-input]]
            [clojure.set :refer [intersection union]]))

(def input (get-partitioned-input "2020/day06.txt"))

(def into-set
  (comp #(into #{} %) seq))

(def count-unions
  (comp count #(apply union %) #(map into-set %)))

(def count-intersections
  (comp count #(apply intersection %) #(map into-set %)))

(def part1 (->> input (map count-unions) (reduce +)))
(def part2 (->> input (map count-intersections) (reduce +)))

(defn -main []
  (println "Advent of Code 2020-06.1:" part1)
  (println "Advent of Code 2020-06.2:" part2))
