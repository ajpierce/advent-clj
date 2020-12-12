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
  (cond (west? dir) :N (north? dir) :E (east? dir) :S (south? dir) :W))
(defn turn-left [dir] (nth (iterate turn-right dir) 3))

(defn turn [bearing dir deg]
  (let [f (cond (= 180 deg) (comp turn-right turn-right)
                (left? dir) turn-left
                (right? dir) turn-right)
        times (case deg 180 1 90 1 270 3 :else 4)]
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
          (recur new-dir (+ x dx) (+ y dy) (rest remaining))))))

;; Given waypoint coords and a distance, returns [dx dy] of the move operation
(defn move-forward [[x y] n] [(* 10 n) (* n y)])
(defn rotate-cw [[x y]] [(* -1 y) x])
(defn rotate-ccw [coords] (nth (iterate rotate-cw coords) 3))

(-> [3 1] (#(iterate rotate-cw %)) (nth 2))
(-> [3 1] (#(iterate rotate-ccw %)) (nth 1))

(defn rotate [dir deg waypoint-coords]
  (let [f (cond (= 180 deg) (comp rotate-cw rotate-cw)
                (left? dir) rotate-ccw
                (right? dir) rotate-cw)
        times (case deg 180 1 90 1 270 3 :else 4)]
    (println "rotating!" dir deg times)
    (nth (iterate f waypoint-coords) times)))

(def part2
  (loop [[sx sy] [0 0] [wx wy] [10 1] remaining (map parse samp)]
    (if (empty? remaining) (+ (Math/abs sx) (Math/abs sy))
        (let [[d n] (first remaining)
              [dsx dsy] (if (forward? d) (move-forward [wx wy] n) [0 0])
              dwx (cond (east? d) n
                        (west? d) (* -1 n)
                        :else 0)
              dwy (cond (north? d) n
                        (south? d) (* -1 n)
                        :else 0)
              new-waypoint (if (contains? #{:R :L} d)
                             (rotate d n [wx wy])
                             [(+ wx dwx) (+ wy dwy)])]
          (println (first remaining) "= sx" sx "sy" sy "dsx" dsx "dsy" dsy "wx" wx "wy" wy)
          (recur [(+ sx dsx) (+ sy dsy)] new-waypoint (rest remaining))))))

(defn -main []
  (println "Advent of Code 2020.12.1:" part1)
  (println "Advent of Code 2020.12.2:" part2))

