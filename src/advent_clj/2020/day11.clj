(ns advent-clj.2020.day11
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.string :as s]))

(defn parse [^String x]
  (->> x seq (map #(if (= \L %) :empty nil)) vec))

(def input (->> (get-puzzle-input "2020/day11.txt") (map parse) vec))
(def sample-input (->> (get-puzzle-input "2020/day11-sample.txt") (map parse) vec))

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(defn count-neighbors [seats loc]
  (let [n (map #(get-in seats %) (neighbors loc))]
    (count (filter #(= % :occupied) n))))

(defn sit-or-split [seats loc]
  (let [current-seat       (get-in seats loc)
        occupied-neighbors (count-neighbors seats loc)]
    (cond (and (= :empty current-seat) (= 0 occupied-neighbors))      :occupied
          (and (= :occupied current-seat) (>= occupied-neighbors 4))  :empty
          :else current-seat)))

(defn step [seats]
  (into [] (for [i (range (count seats))]
             (into [] (for [j (range (count (get seats i)))]
                        (sit-or-split seats [i j]))))))

(def part1
  (let [prev-state (atom nil)]
    (->> (iterate step input)
         (take-while (fn [x] (if (not= @prev-state x)
                               (do (reset! prev-state x) true)
                               false)))
         (last)
         (flatten)
         (filter #(= :occupied %))
         (count))))

(defn -main []
  (println "Advent of Code 2020.11.1:" part1)
  #_(println "Advent of Code 2020.11.2:" part2))
