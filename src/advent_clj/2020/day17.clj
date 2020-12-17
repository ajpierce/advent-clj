(ns advent-clj.2020.day17
  (:require [advent-clj.core :refer [get-puzzle-input]]))

(def input (get-puzzle-input "2020/day17.txt"))
(def samp '(".#." "..#" "###"))

(defn parse
  ([^Integer y ^String row] (parse false y row))
  ([hyper? y row]
   (->> row seq
        (map-indexed
         (fn [x v] (let [coords (if hyper? [0 0 y x] [0 y x])]
                     [coords (if (= \# v) :on :off)]))))))

(defn init-universe [raw-input]
  (->> raw-input
       (map-indexed parse)
       flatten (partition 4)
       (reduce (fn [acc [z y x v]]
                 (let [k [z y x]] (assoc acc k v))) (sorted-map))))

(defn adjacent [n] (range (dec n) (+ 2 n)))
(defn regular-neighbors [[z y x]]
  (for [x' (adjacent x)
        y' (adjacent y)
        z' (adjacent z)
        :when (not= [z y x] [z' y' x'])]
    [z' y' x']))

(defn active? [v] (= :on v))
(defn active-neighbors [neighbors universe coords]
  (->> coords
       neighbors
       (map #(get universe %))
       (filter active?)
       count))

(defn step [neighbors universe]
  (let [cubes-to-change (reduce #(apply conj %1 %2) (sorted-set) (map neighbors (keys universe)))]
    (loop [uu (sorted-map) remaining cubes-to-change]
      (if (empty? remaining) uu
          (let [current-cube (first remaining)
                current-state (get universe current-cube)
                nearby-active (active-neighbors neighbors universe current-cube)
                new-state (cond (and (active? current-state)
                                     (or (= 2 nearby-active) (= 3 nearby-active))) :on
                                (and (not (active? current-state)) (= 3 nearby-active)) :on
                                :else :off)]
            (recur (assoc uu current-cube new-state) (rest remaining)))))))

(def part1
  (->> (nth (iterate (partial step regular-neighbors) (init-universe input)) 6)
       (vals)
       (filter active?)
       (count)))

(defn hyper-neighbors [[h z y x]]
  (for [x' (adjacent x)
        y' (adjacent y)
        z' (adjacent z)
        h' (adjacent h)
        :when (not= [h z y x] [h' z' y' x'])]
    [h' z' y' x']))

(defn init-4d-universe [raw-input]
  (->> raw-input
       (map-indexed (partial parse true))
       flatten (partition 5)
       (reduce (fn [acc [h z y x v]]
                 (let [k [h z y x]] (assoc acc k v))) (sorted-map))))

(def part2
  (->> (nth (iterate (partial step hyper-neighbors) (init-4d-universe input)) 6)
       (vals)
       (filter active?)
       (count)))

(defn -main []
  (println "Advent of Code 2020.17.1:" part1)
  (println "Advent of Code 2020.17.2:" part2))
