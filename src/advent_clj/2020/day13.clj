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

(defn parse2 [in]
  (->> (s/split (last in) #",")
       (map-indexed (fn [idx busid]
                      (try [idx (Integer/parseInt busid)]
                           (catch Exception _ nil))))
       (remove nil?)))

(defn time-warp
  "Returns the next available time that satisfies the current bus/offset.
  Step size is determined by all busses prior; see part2 below."
  [t step offset busid]
  (->> (iterate (partial + step) (+ t step))
       (filter #(zero? (mod (+ % offset) busid)))
       first))

(def part2
  (loop [t 0 step 1 remaining (parse2 input)]
    (if (empty? remaining) t
        (let [[offset busid] (first remaining)]
          (recur (time-warp t step offset busid)
                 (* step busid)
                 (rest remaining))))))

(defn -main []
  (println "Advent of Code 2020.13.1:" part1)
  (println "Advent of Code 2020.13.2:" part2))
