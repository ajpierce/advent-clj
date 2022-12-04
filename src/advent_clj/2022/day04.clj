(ns advent-clj.2022.day04
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.set :refer [subset? intersection]]
            [clojure.string :refer [split]])
  (:gen-class))

(def input (get-puzzle-input "2022/day04.txt"))
(def assignment-regex #"(\d+)-(\d+),(\d+)-(\d+)")
(defn inclusive-range [[a b]] (range a (inc b)))

(def get-sections (comp (partial map (comp set inclusive-range))
                        (partial partition 2)
                        (partial map read-string)
                        rest
                        (partial re-matches assignment-regex)))

(defn overlaps? [[s1 s2]] (or (subset? s1 s2) (subset? s2 s1)))
(defn intersects? [[s1 s2]] (not (empty? (intersection s1 s2))))

(def part1 (->> input (map get-sections) (filter overlaps?) count))
(def part2 (->> input (map get-sections) (filter intersects?) count))

(defn -main []
  (println "Advent of Code 2022-04.1:" part1)
  (println "Advent of Code 2022-04.2:" part2))
