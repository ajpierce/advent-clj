(ns advent-clj.2020.day06
  (:require [advent-clj.core :refer [get-partitioned-input]]
            [clojure.set :refer [intersection union]]))

(def input (get-partitioned-input "2020/day06.txt"))

(defn into-sets [group]
  (->> group (map seq) (map #(into #{} %))))

(def count-set-unions
  (comp count #(apply union %) into-sets))

(def count-set-intersections
  (comp count #(apply intersection %) into-sets))

(def part1 (->> input (map count-set-unions) (reduce +)))
(def part2 (->> input (map count-set-intersections) (reduce +)))

(defn -main []
  (println "Advent of Code 2020-06.1:" part1)
  (println "Advent of Code 2020-06.2:" part2))
