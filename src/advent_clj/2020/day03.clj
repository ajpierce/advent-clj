(ns advent-clj.2020.day03
  (:require [advent-clj.core :refer [get-puzzle-input]])
  (:gen-class))

(def input (get-puzzle-input "2020/day03.txt"))

(defn traverse [dx dy]
  (loop [idx 0 slope input trees 0]
    (if (empty? slope) trees
        (let [row (cycle (first slope))
              t (if (= \# (nth row idx)) 1 0)]
          (recur (+ dx idx) (nthrest slope dy) (+ trees t))))))

(def part1 (traverse 3 1))

(def part2
  (let [deltas [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (->> (map #(apply traverse %) deltas)
         (reduce *))))

(defn -main []
  (println "Advent of Code 2020-03.1:" part1)
  (println "Advent of Code 2020-03.2:" part2))
