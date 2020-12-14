(ns advent-clj.2020.day14
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.set :refer [union]]))

(def input (get-puzzle-input "2020/day14.txt"))
(def samp (get-puzzle-input "2020/day14-sample.txt"))
(def samp2 (get-puzzle-input "2020/day14-sample2.txt"))

(defn apply-mask
  ([mask x] (apply-mask true mask x))
  ([xf-zero? mask x]
   (let [n (Integer/parseInt x)
         transforms (->> mask seq reverse
                         (map-indexed #(case %2
                                         \1 [%1 bit-set]
                                         \0 (if xf-zero? [%1 bit-clear] nil)
                                         nil))
                         (remove nil?))]
     (reduce (fn [acc [i xf]] (xf acc i)) n transforms))))

(def part1
  (loop [mem {} mask "" cmds input]
    (if (empty? cmds) (reduce + (vals mem))
        (let [cmd (first cmds)
              [_ address x :as assign] (re-matches #"mem\[(\d+)\] = (\d+)" cmd)
              [_ new-mask] (re-matches #"^mask = (.+)$" cmd)]
          (if (nil? assign)
            (recur mem new-mask (rest cmds))
            (recur (assoc mem address (apply-mask mask x)) mask (rest cmds)))))))

(defn apply-address-mask
  "Returns a list of all possible addresses to which a value should be written"
  [mask address]
  (let [a (apply-mask false mask address)
        wildcards (->> mask seq reverse vec
                       (reduce-kv #(case %3 \X (conj %1 %2) %1) []))]
    (loop [as #{a} xs wildcards]
      (if (empty? xs) as
          (let [x (first xs)
                as' (reduce #(conj %1 (bit-set %2 x) (bit-clear %2 x)) #{} as)]
            (recur (union as as') (rest xs)))))))

(def part2
  (loop [mem {} mask "" cmds input]
    (if (empty? cmds) (reduce + (vals mem))
        (let [cmd (first cmds)
              [_ address x :as assign] (re-matches #"mem\[(\d+)\] = (\d+)" cmd)
              [_ new-mask] (re-matches #"^mask = (.+)$" cmd)]
          (if (nil? assign)
            (recur mem new-mask (rest cmds))
            (let [addresses (apply-address-mask mask address)]
              (recur (reduce #(assoc %1 %2 (Integer/parseInt x)) mem addresses) mask (rest cmds))))))))

(defn -main []
  (println "Advent of Code 2020.14.1:" part1)
  (println "Advent of Code 2020.14.2:" part2))
