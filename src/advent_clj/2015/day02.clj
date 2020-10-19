(ns advent-clj.2015.day02
  (:require [advent-clj.core :refer [get-puzzle-input]])
  (:gen-class))

(def input (get-puzzle-input "2015/day02.txt"))

(defn split-sort [s] 
  (->> (clojure.string/split s #"x")
       (map #(Integer/parseInt %))
       sort))

(def sorted-input (map split-sort input))

(defn calc-paper [[^int a ^int b ^int c]]
  (+ (* 3 a b) (* 2 b c) (* 2 a c)))

(def part1
  (time
  (reduce + (map calc-paper sorted-input))))

(defn calc-ribbon [[^int a ^int b ^int c]]
  (+ (* 2 a) (* 2 b) (* a b c)))

(def part2
  (time 
    (reduce + (map calc-ribbon sorted-input))))

(println "2015 Day 02, Part 1: " part1)
(println "2015 Day 02, Part 2: " part2)
