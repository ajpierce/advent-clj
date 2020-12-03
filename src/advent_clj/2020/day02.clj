(ns advent-clj.2020.day02
  (:require [advent-clj.core :refer [get-puzzle-input]])
  (:gen-class))

(def input (get-puzzle-input "2020/day02.txt"))

(defn format-entry [x]
  (let [[rule letter pass] (clojure.string/split x #" ")]
    (list (map #(Integer/parseInt %) (clojure.string/split rule #"-"))
          (first letter)
          pass)))

(defn valid-pass?
  "Checks to see if the formatted entry is valid accoring to part 1"
  [[[mn mx] letter pass]]
  (let [occurrences (get (frequencies pass) letter)]
    (if (nil? occurrences)
      false
      (and (>= occurrences mn) (<= occurrences mx)))))

(defn valid-pass??
  "Checks to see if the formatted entry is valid accoring to part 2"
  [[[mn mx] letter pass]]
  (let [one (get pass (dec mn))
        two (get pass (dec mx))]
    (and (not-every? #(= letter %) [one two])
         (some #(= letter %) [one two]))))

(def part1 (->> input (map format-entry) (filter valid-pass?) count))
(def part2 (->> input (map format-entry) (filter valid-pass??) count))

(defn -main []
  (println "Advent of Code 2020-02.1:" part1)
  (println "Advent of Code 2020-02.2:" part2))
