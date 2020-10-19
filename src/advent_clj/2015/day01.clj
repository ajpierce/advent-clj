(ns advent-clj.2015.day01
  (:require [advent-clj.core :refer [get-puzzle-input]])
  (:gen-class))

(def input (get-puzzle-input "2015/day01.txt"))

(defn parse-char [c]
  (case c
        \( 1
        \) -1
        :else 0))

(def part1
  (time
   (->> input
        (map parse-char)
        (reduce +))))

(def part2
  (time
   (loop [floor 0 steps input]
     (let [[step & remaining] steps
           d (parse-char step)
           current (+ floor d)]
       (if (= -1 floor)
         (- (count input) (count steps))
         (recur current remaining))))))

(println "2015 Day 01, Part 1: " part1)
(println "2015 Day 01, Part 2: " part2)
