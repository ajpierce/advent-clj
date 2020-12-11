(ns advent-clj.2020.day10
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.set :refer [difference union]]
            [clojure.core.reducers :as r]))

(def input
  (->> (get-puzzle-input "2020/day10.txt")
       (map #(Integer/parseInt %))))

(defn with-outlets
  ([ps]   (with-outlets input ps))
  ([i ps] (sort (conj ps 0 (+ 3 (apply max i))))))

(def part1
  (time
   (->> (with-outlets input)
        (partition 2 1)
        (map (fn [[a b]] (- b a)))
        (frequencies)
        (vals)
        (reduce *))))

(defn get-paths [joltages]
  (reduce (fn [acc [x y]] (update acc y conj x))
          (sorted-map)
          (for [x joltages
                y (rest joltages)
                :when (and (< x y) (<= (- y x) 3))]
            [x y])))

(defn path-length-reducer [acc [k vs]]
  (assoc acc k (apply + (map #(get acc % 1) vs))))

(def part2
  (time
   (->> (with-outlets input)
        (get-paths)
        (reduce path-length-reducer (sorted-map))
        last val)))

(defn -main []
  (println "Advent of Code 2020.10.1:" part1)
  (println "Advent of Code 2020.10.2:" part2))



;; Scratch pad
(comment
  (def test-input
    (->> (get-puzzle-input "2020/day10-test.txt")
         (map #(Integer/parseInt %))))

  (def sample-input
    (->> (get-puzzle-input "2020/day10-sample.txt")
         (map #(Integer/parseInt %))))

  (def with-sample-outlets (partial with-outlets sample-input))
  (def sample-outlets (with-sample-outlets sample-input))

  (def with-test-outlets (partial with-outlets test-input))
  (def test-outlets (with-test-outlets test-input)))
