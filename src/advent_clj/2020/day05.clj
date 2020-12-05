(ns advent-clj.2020.day05
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.string :as s])
  (:gen-class))

(def input (get-puzzle-input "2020/day05.txt"))

(defn to-binary [^Character c]
  (case c \B 1 \F 0 \R 1 \L 0))

(def joincast (comp #(Integer/parseInt % 2)
                    #(apply str %)))

(defn get-id [[row col]]
  (+ col (* row 8)))

(defn parse-pass [^String p]
  (->> p
       (map to-binary)
       (split-at 7)
       (map joincast)
       (get-id)))

(def part1 (->> input (map parse-pass) (apply max)))

(def part2
  (loop [remaining (->> input (map parse-pass) sort)]
    (let [[a b & others] remaining]
      (cond
        (empty? remaining) nil
        (> (- b a) 1) (- b 1)
        :else (recur (cons b others))))))

(defn -main []
  (println "Advent of Code 2020-05.1:" part1)
  (println "Advent of Code 2020-05.2:" part2))
