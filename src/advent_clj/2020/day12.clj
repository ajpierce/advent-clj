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

(defn turn [xf dir deg arg]
  (let [times (if (= 270 deg) 3 1)
        f (cond (= 180 deg) (comp xf xf)
                (right? dir) xf
                (left? dir) (comp xf xf xf))]
    (nth (iterate f arg) times)))

(defn to-starboard [dir]
  (cond (west? dir) :N (north? dir) :E (east? dir) :S (south? dir) :W))

(def part1
  (loop [bearing :E x 0 y 0 remaining (map parse input)]
    (if (empty? remaining) (+ (Math/abs x) (Math/abs y))
        (let [[d n] (first remaining)
              dx (cond (or (east? d) (and (east? bearing) (forward? d))) n
                       (or (west? d) (and (west? bearing) (forward? d))) (* -1 n)
                       :else 0)
              dy (cond (or (south? d) (and (south? bearing) (forward? d))) n
                       (or (north? d) (and (north? bearing) (forward? d))) (* -1 n)
                       :else 0)
              new-bearing (if (contains? #{:R :L} d) (turn to-starboard d n bearing) bearing)]
          (recur new-bearing (+ x dx) (+ y dy) (rest remaining))))))

(defn ahead-full [[x y] n] [(* n x) (* n y)])
(defn rotate-cw [[x y]] [y (* -1 x)])

(def part2
  (loop [[sx sy] [0 0] [wx wy] [10 1] remaining (map parse input)]
    (if (empty? remaining) (+ (Math/abs sx) (Math/abs sy))
        (let [[d n] (first remaining)
              [dsx dsy] (if (forward? d) (ahead-full [wx wy] n) [0 0])
              dwx (cond (east? d) n
                        (west? d) (* -1 n)
                        :else 0)
              dwy (cond (north? d) n
                        (south? d) (* -1 n)
                        :else 0)
              new-waypoint (if (contains? #{:R :L} d)
                             (turn rotate-cw d n [wx wy])
                             [(+ wx dwx) (+ wy dwy)])]
          (recur [(+ sx dsx) (+ sy dsy)] new-waypoint (rest remaining))))))

(defn -main []
  (println "Advent of Code 2020.12.1:" part1)
  (println "Advent of Code 2020.12.2:" part2))
