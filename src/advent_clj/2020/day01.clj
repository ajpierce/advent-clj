(ns advent-clj.2020.day01
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.math.combinatorics :refer [combinations]])
  (:gen-class))

(def input (->> (get-puzzle-input "2020/day01.txt")
                (map #(Integer/parseInt %))))

(defn solve [n]
    (->> (combinations input n)
         (filter #(= 2020 (apply + %)))
         (first)
         (reduce *)))

(def part1 (time (solve 2)))
(def part2 (time (solve 3)))

(defn -main []
  (println "Advent of Code 2020-01.1:" part1)
  (println "Advent of Code 2020-01.2:" part2))




(comment
  "Here was my first implementation of part1; it was an order of magnitude
  faster than using combinatorics (3ms instead of 30ms)."

(def part1-fast
   (loop [i (first input)
          remaining (rest input)]
     (let [j (first (for [x remaining :when (= 2020 (+ i x))] x))]
       (if (some? j)
         (* i j)
         (recur (first remaining) (rest remaining))))))
)
