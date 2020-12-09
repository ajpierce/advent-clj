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
    (if (> subset-size (count input)) nil
        (let [consec-nums (partition subset-size 1 input)
              matching (filter #(= part1 (reduce + %)) consec-nums)]
          (if (empty? matching) (recur (inc subset-size))
              (+ (apply min (flatten matching)) (apply max (flatten matching))))))))

(defn -main []
  (println "Advent of Code 2020-09.1:" part1)
  (println "Advent of Code 2020-09.2:" part2))


(comment
  "Here was my original solution to part 2. It was slower"
  (def part2
    (->> (for [subset-size (range 2 21)  ;; 20 is an arbitrary max subset size to keep it fast
               :let [consec-nums (partition subset-size 1 input)
                     matching (filter #(= part1 (reduce + %)) consec-nums)]
               :when (not (empty? matching))]
           matching)
         flatten
         (#(+ (apply min %) (apply max %)))))
)
