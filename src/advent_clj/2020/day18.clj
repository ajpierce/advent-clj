(ns advent-clj.2020.day18
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.string :as s]
            [clojure.java.shell :as shell])
  (:gen-class))

(defn parse
  ([^String x] (parse identity x))
  ([xf ^String x] (read-string (xf (str "(" x ")")))))

(def input (map parse (get-puzzle-input "2020/day18.txt")))

(defn unwrap [restargs]
  (if (= 1 (count restargs))
    (first restargs)
    restargs))

(defn postfix-me [expr]
  (if (list? expr)
    (let [[arg1 op & arg2] expr]
      `(~op ~(postfix-me arg1) ~(postfix-me (unwrap arg2))))
    expr))

(defn reverse-list [x]
  (cond (and (seq? x) (= 1 (count x))) (first x)
        (seq? x) (reverse x)
        :else x))

(def solve (comp eval postfix-me #(clojure.walk/postwalk reverse-list %)))

(def part1 (->> input (map solve) (reduce +)))

(def input2
  (-> (get-puzzle-input "2020/day18.txt")
      (#(s/join ")+(" %))
      (s/replace #"\*" ") * (")
      (#(str "(" % ")"))))

;; I got tired of translating infix notation so I... outsourced it 😅
;; This solution requires that you have node installed on your computer
(def part2 (:out (shell/sh "node" "-p" input2)))

(defn -main []
  (println "Advent of Code 2020.18.1:" part1)
  (println "Advent of Code 2020.18.2:" part2))
