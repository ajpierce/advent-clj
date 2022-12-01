(ns advent-clj.2022.day01
  (:require [advent-clj.core :refer [get-puzzle-input]])
  (:gen-class))

(def input (->> (get-puzzle-input "2022/day01.txt")
                (partition-by empty?)
                (remove (comp empty? first))
                (map #(map read-string %))))

(def part1 (time (->> input (map #(reduce + %)) (apply max))))
(def part2 (time (->> input (map #(reduce + %)) sort reverse (take 3) (reduce +))))

(defn -main []
  (println "Advent of Code 2022-01.1:" part1)
  (println "Advent of Code 2022-01.2:" part2))

(comment "")
