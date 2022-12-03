(ns advent-clj.2022.day03
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.set :refer [intersection]]
            [clojure.string :refer [index-of]])
  (:gen-class))

(def input (get-puzzle-input "2022/day03.txt"))
(def items "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def priority (comp inc (partial index-of items)))

(def find-error (comp (partial apply intersection)
                      (partial map (partial into #{}))))

(defn halve [s] (split-at (/ (count s) 2) s))

(def solve (comp (partial reduce +)
                 (partial map (comp priority first find-error))))

(def part1 (solve (map halve input)))
(def part2 (solve (partition 3 input)))

(defn -main []
  (println "Advent of Code 2022-03.1:" part1)
  (println "Advent of Code 2022-03.2:" part2))
