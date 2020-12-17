(ns advent-clj.2020.day17
  (:require [advent-clj.core :refer [get-puzzle-input]]))

(defn parse [^Integer y ^String row]
  (->> row seq
       (map-indexed
        (fn [x v] (let [coords [0 y x]]
                    [coords (if (= \# v) :on :off)])))))

(defn init-universe [raw-input]
  (->> raw-input
       (map-indexed parse)
       flatten (partition 4)
       (reduce (fn [acc [z y x v]]
                 (let [k [z y x]] (assoc acc k v))) (sorted-map))))

(def input (init-universe (get-puzzle-input "2020/day17.txt")))
(def samp (init-universe '(".#." "..#" "###")))

(defn adjacent [n] (range (dec n) (+ 2 n)))
(defn neighbors [[z y x]]
  (for [x' (adjacent x)
        y' (adjacent y)
        z' (adjacent z)
        :when (not= [z y x] [z' y' x'])]
    [z' y' x']))

(defn active? [v] (= :on v))
(defn active-neighbors [universe coords]
  (->> coords
       neighbors
       (map #(get universe %))
       (filter active?)
       count))

(defn step [universe]
  (let [cubes-to-change (reduce #(apply conj %1 %2) (sorted-set) (map neighbors (keys universe)))]
    (loop [uu (sorted-map) remaining cubes-to-change]
      (if (empty? remaining) uu
          (let [current-cube (first remaining)
                current-state (get universe current-cube)
                nearby-active (active-neighbors universe current-cube)
                new-state (cond (and (active? current-state)
                                     (or (= 2 nearby-active) (= 3 nearby-active))) :on
                                (and (not (active? current-state)) (= 3 nearby-active)) :on
                                :else :off)]
            #_(println current-cube "is" current-state ", has" nearby-active "neighbors. Becomes:" new-state)
            (recur (assoc uu current-cube new-state) (rest remaining)))))))

(def part1
  (->> (nth (iterate step input) 6)
       (vals)
       (filter active?)
       (count)))
