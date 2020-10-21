(ns advent-clj.2015.day05
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.string :as s])
  (:gen-class))

(def input (get-puzzle-input "2015/day05.txt"))

(def vowel?
  (memoize
   (fn [^Character c] (contains? #{\a \e \i \o \u} c))))

(def invalid-substrings #{ "ab" "cd" "pq" "xy" })

(def invalid? (comp
               #(contains? invalid-substrings %)
               #(s/join "" %)))

(defn duplicate? [[a b]] (= a b))

(defn judge-word [^String s]
  (let [vowels (count (filter vowel? s))
        pairs (partition 2 1 s)]
    (and (>= vowels 3)
         (= (count (filter invalid? pairs)) 0)
         (> (count (filter (fn [[a b]] (= a b)) pairs)) 0))))

(def part1 (time (->> input (filter judge-word) count)))

(defn remove-triples [^String s]
  (s/replace s #"(\w)\1{2}" "$1$1"))

(def one-or-more? (comp pos? dec))

(defn has-pairs? [^String s]
  (->> s
       remove-triples
       (partition 2 1)
       frequencies
       vals
       (filter one-or-more?)
       count
       pos?))

(defn has-spaced-repeat? [^String s]
  (let [triples (partition 3 1 s)]
    (pos? (count (filter (fn [[a _ c]] (= a c)) triples)))))

(defn nice? [^String s]
  (and (has-spaced-repeat? s)
       (has-pairs? s)))

(def part2 (time (->> input (filter nice?) count)))

(defn -main []
  (println "2015 Day 05, Part 1: " part1)
  (println "2015 Day 05, Part 2: " part2))
