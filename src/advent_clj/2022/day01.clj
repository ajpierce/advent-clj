(ns advent-clj.2022.day01
  (:require [advent-clj.core :refer [get-puzzle-input]])
  (:gen-class))

(def sum-as-ints (comp (partial reduce +)
                       (partial map read-string)))

(def sorted-sums (->> (get-puzzle-input "2022/day01.txt")
                      (partition-by empty?)
                      (remove (comp empty? first))
                      (map sum-as-ints)
                      sort reverse))

(def part1 (first sorted-sums))
(def part2 (reduce + (take 3 sorted-sums)))

(defn -main []
  (println "Advent of Code 2022-01.1:" part1)
  (println "Advent of Code 2022-01.2:" part2))
