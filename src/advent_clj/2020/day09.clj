(ns advent-clj.2020.day09
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.math.combinatorics :refer [combinations]]))

(def input (->> (get-puzzle-input "2020/day09.txt")
                (map #(Long/parseLong %))))

(defn make-sums [digits]
  (->> (combinations digits 2)
       (map #(apply + %))
       (set)))

(defn valid? [digits]
  (contains? (make-sums (butlast digits)) (last digits)))

(def part1
  (->> (partition 26 1 input)
       (remove valid?)
       first last))

(def part2
  (loop [subset-size 2]
    (let [consec-nums (partition subset-size 1 input)
          matching (first (filter #(= part1 (reduce + %)) consec-nums))]
      (cond (> subset-size (count input)) nil
            (some some? matching) (+ (apply min matching) (apply max matching))
            :else (recur (inc subset-size))))))

(defn -main []
  (println "Advent of Code 2020-09.1:" part1)
  (println "Advent of Code 2020-09.2:" part2))
