(ns advent-clj.2015.day06
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.string :as s])
  (:gen-class))

(def input (get-puzzle-input "2015/day06.txt"))

(defn off ([] false) ([_] false))
(defn on ([] true) ([_] true))
(defn toggle ([] true) ([x] (not x)))

(def split-parse (comp (fn [x] (map #(Integer/parseInt %) x))
                       #(s/split % #",")))

(defn scrub [terms] (remove #(or (= % "turn") (= % "through")) terms))

(defn get-fn [s]
  (case s "off" off "on" on "toggle" toggle identity))

(defn transform [transformer [instruction start end]]
  (let [[x1, y1] (split-parse start)
        [x2, y2] (split-parse end)]
    [(transformer instruction) [x1 x2] [y1 y2]]))

(defn get-instructions [[f xs ys]]
  (for [x (range (first xs) (inc (last xs)))
        y (range (first ys) (inc (last ys)))
        :let [coords (str x "," y)]]
    [coords f]))

(def clean (comp get-instructions
                 (partial transform get-fn)
                  scrub
                 #(s/split % #" ")))

(defn flip-switch [m [coords f]] (update m coords f))

(def part1
  (time
   (->> input
        (map clean)
        (apply concat)
        (reduce flip-switch {})
        vals
        (filter true?)
        count)))

(defn down
  ([] 0)
  ([x] (if (nil? x) 0 (max 0 (dec x)))))

(defn up
  ([] 1)
  ([x] (if (nil? x) 1 (inc x))))

(defn two
  ([] 2)
  ([x] (if (nil? x) 2 (+ 2 x))))

(defn get-fn2 [s]
  (case s "off" down "on" up "toggle" two identity))

(def clean2 (comp get-instructions
                  (partial transform get-fn2)
                  scrub
                  #(s/split % #" ")))

(def part2
  (time
   (->> input
        (map clean2)
        (apply concat)
        (reduce flip-switch {})
        (vals)
        (reduce +))))

(defn -main []
  (println "2015 Day 05, Part 1: " part1)
  (println "2015 Day 05, Part 2: " part2))
