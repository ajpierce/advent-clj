(ns advent-clj.2020.day07
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.string :as s]))

(def input (get-puzzle-input "2020/day07.txt"))

(def clean (comp #(map s/trim %)
                 #(s/split % #"( contain |,)")
                 #(s/replace % #"(bag(s?)|\.)" "")))

(defn parse-bag [^String bag]
  (let [[_ qty color] (re-matches #"^(\d{1,2})? (.*)$" bag)]
    [color (if (some? qty) (#(Integer/parseInt qty)) 0)]))

(defn just-bags-reducer [acc [bag & contents]]
  (assoc acc bag (set (map (comp first parse-bag) contents))))

(def just-bags (->> input (map clean) (reduce just-bags-reducer {})))

(defn get-containers [^String bag]
  (->> just-bags
       (filter (fn [[_ v]] (contains? v bag)))
       keys))

(def part1
  (loop [eligible #{} to-process ["shiny gold"]]
    (if (empty? to-process) (count eligible)
        (let [containers (get-containers (first to-process))]
          (recur (apply conj eligible containers)
                 (concat (rest to-process) containers))))))

(defn quantified-bags-reducer [acc [bag & contents]]
  (assoc acc bag (map parse-bag contents)))

(def quantified-bags (->> input (map clean) (reduce quantified-bags-reducer {})))

(defn get-contents [bag]
  (let [contents (get quantified-bags bag)]
    (if (= '([nil 0]) contents) 0
        (map (fn [[color, qty]] (if (some? color) [(get-contents color) qty] 0)) contents))))

(defn compute-qty [contents]
  (cond
    (number? contents) contents
    (vector? contents) (+ (last contents) (* (last contents) (compute-qty (first contents))))
    (seq? contents) (apply + (map compute-qty contents))
    :else 0))

(defn -main []
  (println "Advent of Code 2020-07.1:" part1)
  (println "Advent of Code 2020-07.2:" (compute-qty (get-contents "shiny gold"))))
