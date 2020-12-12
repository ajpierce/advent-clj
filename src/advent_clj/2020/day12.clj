(ns advent-clj.2020.day12
  (:require [advent-clj.core :refer [get-puzzle-input]]))

(defn parse [x]
  (let [[_ c n] (re-matches #"^(\w)(\d+)$" x)]
    [(keyword c) (Integer/parseInt n)]))

(def input (get-puzzle-input "2020/day12.txt"))
(def samp (get-puzzle-input "2020/day12-sample.txt"))

(def east? (partial = :E))
(def west? (partial = :W))
(def south? (partial = :S))
(def north? (partial = :N))
(def left? (partial = :L))
(def right? (partial = :R))
(def forward? (partial = :F))

(defn turn-right [dir]
  (cond (west? dir) :N
        (north? dir) :E
        (east? dir) :S
        (south? dir) :W))

(defn about-face [dir] (nth (iterate turn-right dir) 2))
(defn turn-left [dir] (nth (iterate turn-right dir) 3))

(defn turn [bearing dir deg]
  (let [f (cond (= 180 deg) about-face
                (left? dir) turn-left
                (right? dir) turn-right)
        times (case deg 180 1 90 1 270 3)]
    (nth (iterate f bearing) times)))

(def part1
  (loop [dir :E x 0 y 0 remaining (map parse input)]
    (if (empty? remaining) (+ (Math/abs x) (Math/abs y))
        (let [[d n] (first remaining)
              dx (cond (or (east? d) (and (east? dir) (forward? d))) n
                       (or (west? d) (and (west? dir) (forward? d))) (* -1 n)
                       :else 0)
              dy (cond (or (south? d) (and (south? dir) (forward? d))) n
                       (or (north? d) (and (north? dir) (forward? d))) (* -1 n)
                       :else 0)
              new-dir (if (contains? #{:R :L} d) (turn dir d n) dir)]
          #_(println "x" x "y" y "dx" dx "dy" dy "dir" dir "new-dir" new-dir)
          (recur new-dir (+ x dx) (+ y dy) (rest remaining))))))
