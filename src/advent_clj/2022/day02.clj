(ns advent-clj.2022.day02
  (:require [advent-clj.core :refer [get-puzzle-input]])
  (:gen-class))

(def input (->> (get-puzzle-input "2022/day02.txt")
                (map (comp (partial remove (partial = \space)) seq))))

(def choices (cycle '(:rock :paper :scissors)))
(def get-hand (partial nth choices))
(def lookup {\A 0 \B 1 \C 2 \X 0 \Y 1 \Z 2})

(def wins #{'(:rock :paper) '(:paper :scissors) '(:scissors :rock)})
(def win? (partial contains? wins))
(def draw? (partial apply =))

(def p1-translator (partial map (comp get-hand lookup)))
(defn p2-translator [[elf me]]
  (let [offset (case me \X 2 \Y 0 \Z 1)
        base (lookup elf)]
    [(get-hand base) (get-hand (+ base offset))]))

(def points {:rock 1 :paper 2 :scissors 3})
(defn score [hand]
  (+ (points (last hand))
     (cond (win? hand) 6 (draw? hand) 3 :else 0)))

(defn solve [translator]
  (->> input
       (map (comp score translator))
       (reduce +)))

(def part1 (solve p1-translator))
(def part2 (solve p2-translator))

(defn -main []
  (println "Advent of Code 2022-02.1:" part1)
  (println "Advent of Code 2022-02.2:" part2))
