(ns advent-clj.2020.day08
  (:require [advent-clj.core :refer [get-puzzle-input]]))

(defn parse [^String x]
  (let [[_ inst delta] (re-matches #"^(\w{3}) (.*)$" x)]
    [inst (Integer/parseInt delta)]))

(def input (->> (get-puzzle-input "2020/day08.txt") (map parse) vec))

(def part1
  (loop [acc 0 idx 0 seen #{}]
    (if (contains? seen idx) acc
        (let [[inst delta] (get input idx)]
          (cond (= "acc" inst) (recur (+ acc delta) (inc idx)     (conj seen idx))
                (= "jmp" inst) (recur acc           (+ idx delta) (conj seen idx))
                :else          (recur acc           (inc idx)     (conj seen idx)))))))

(defn run [mod-idx]
  (if (= "acc" (first (get input mod-idx))) nil
      (let [mod-input (update input mod-idx (fn [[i d]] [(if (= "jmp" i) "nop" "jmp") d]))]
        (loop [acc 0 idx 0 seen #{}]
          (cond (contains? seen idx)  nil
                (= idx (count mod-input)) acc
                :else
                (let [[inst delta] (get mod-input idx)]
                  (cond (= "acc" inst) (recur (+ acc delta) (inc idx)     (conj seen idx))
                        (= "jmp" inst) (recur acc           (+ idx delta) (conj seen idx))
                        :else          (recur acc           (inc idx)     (conj seen idx)))))))))

(def part2
  (->> (range 0 (count input))
       (pmap run)
       (filter some?)
       first))

(defn -main []
  (println "Advent of Code 2020-08.1:" part1)
  (println "Advent of Code 2020-08.2:" part2))
