(ns advent-clj.2020.day07
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.string :as s]))

(def clean (comp #(map s/trim %)
                 #(s/split % #"( contain |,)")
                 #(s/replace % #"(bag(s?)|\.)" "")))

(def input (map clean (get-puzzle-input "2020/day07.txt")))

(defn parse-bag [^String bag]
  (let [[_ qty color] (re-matches #"^(\d{1,2})? (.*)$" bag)]
    [color (if (some? qty) (#(Integer/parseInt qty)) 0)]))

(defn bag-reducer [xf]
  (fn [acc [bag & contents]] (assoc acc bag (xf contents))))

(def just-bags (reduce (bag-reducer #(set (map (comp first parse-bag) %))) {} input))

(def quantified-bags (reduce (bag-reducer #(map parse-bag %)) {} input))

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

(defn get-contents [bag]
  (let [contents (get quantified-bags bag)]
    (if (= '([nil 0]) contents) 0
        (map (fn [[color, qty]] (if (some? color) [(get-contents color) qty] 0)) contents))))

(defn compute-qty [contents]
  (cond
    (number? contents) contents
    (vector? contents) (let [[more qty] contents]
                         (+ qty (* qty (compute-qty more))))
    (seq? contents)    (apply + (map compute-qty contents))
    :else 0))

(defn -main []
  (println "Advent of Code 2020-07.1:" part1)
  (println "Advent of Code 2020-07.2:" (compute-qty (get-contents "shiny gold"))))
