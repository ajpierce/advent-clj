(ns advent-clj.2020.day13
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.string :as s]))

(def input (get-puzzle-input "2020/day13.txt"))
(def samp (get-puzzle-input "2020/day13-sample.txt"))

(defn parse-pt1 [in]
  (let [dt (Integer/parseInt (first in))
        busses (->> (s/split (last in) #",")
                    (remove #(= % "x"))
                    (map (comp #(Integer/parseInt %) s/trim)))]
    [dt busses]))

(defn get-departure-time [dt busid]
  (->> (iterate (partial + busid) 0)
       (filter (partial < dt))
       first))

(defn min-bus-reducer [dt acc busid]
  (let [bdt (get-departure-time dt busid) [earliest _] acc]
    (if (or (nil? earliest) (< bdt earliest)) [bdt busid] acc)))

(def part1
  (let [[dt busses] (parse-pt1 input)]
    (->> busses
         (reduce (partial min-bus-reducer dt) [])
         (#(let [[bdt bid] %]
             (* bid (- bdt dt)))))))

(defn -main []
  (println "Advent of Code 2020.13.1:" part1)
  #_(println "Advent of Code 2020.13.2:" part2))
