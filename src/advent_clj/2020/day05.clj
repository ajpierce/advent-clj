(ns advent-clj.2020.day05
  (:require [advent-clj.core :refer [get-puzzle-input]])
  (:gen-class))

(def input (get-puzzle-input "2020/day05.txt"))

(defn to-binary [^Character c]
  (case c \B 1 \F 0 \R 1 \L 0))

(def get-id (comp #(Integer/parseInt % 2)
                  #(apply str %)
                  #(map to-binary %)))

(def part1 (->> input (map get-id) (apply max)))

(def part2 (->> input (map get-id) sort
                (partition 2 1)
                (remove (fn [[a b]] (= 1 (- b a))))
                first first inc))

(defn -main []
  (println "Advent of Code 2020-05.1:" part1)
  (println "Advent of Code 2020-05.2:" part2))
