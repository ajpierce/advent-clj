(ns advent-clj.2015.day03
  (:require [advent-clj.core :refer [get-puzzle-input]])
  (:gen-class))

(def input (get-puzzle-input "2015/day03.txt"))

(defn get-delta [c]
  (case c
    \^ [0 1] \v [0 -1]
    \< [-1 0] \> [1 0]
    :else [0 0]))

(def deltas (map get-delta input))

(def part1
  (time
  (loop [houses [[0 0]] steps deltas]
    (let [[d & remaining] steps
          current (map + (last houses) d)]
      (if (= 0 (count remaining))
        (count (set houses))
        (recur (conj houses current) remaining))))))

(def part2
  (time
   (let [[santa robo-santa] (apply map list (partition 2 deltas))]
     (loop [shouses ['(0 0)]
            rhouses ['(0 0)]
            ssteps santa
            rsteps robo-santa]
       (let [[ds & rs] ssteps
             [dr & rr] rsteps
             cs (map + (last shouses) ds)
             cr (map + (last rhouses) dr)]
         (if (= 0 (count rs))
           (-> (concat (conj shouses cs) (conj rhouses cr)) set count)
           (recur (conj shouses cs) (conj rhouses cr) rs rr)))))))

(defn -main []
  (println "2015 Day 03, Part 1: " part1)
  (println "2015 Day 03, Part 2: " part2))
