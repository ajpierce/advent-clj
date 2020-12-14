(ns advent-clj.2020.day14
  (:require [advent-clj.core :refer [get-puzzle-input]]))

#_(def pp clojure.pprint/pprint)
(def input (get-puzzle-input "2020/day14.txt"))
(def samp (get-puzzle-input "2020/day14-sample.txt"))

(defn apply-mask [mask x]
  (let [n (Integer/parseInt x)
        transforms (->> mask seq reverse
                        (map-indexed #(case %2 \X nil
                                            \1 [%1 bit-set]
                                            \0 [%1 bit-clear]))
                        (remove nil?))]
    (reduce (fn [acc [i xf]] (xf acc i)) n transforms)))

(def part1
  (loop [mem {} mask "" cmds input]
    (if (empty? cmds) (reduce + (vals mem))
        (let [cmd (first cmds)
              [_ address x :as assign] (re-matches #"mem\[(\d+)\] = (\d+)" cmd)
              [_ new-mask] (re-matches #"^mask = (.+)$" cmd)]
          (if (some? assign)
            (recur (assoc mem address (apply-mask mask x)) mask (rest cmds))
            (recur mem new-mask (rest cmds)))))))

(defn -main []
  (println "Advent of Code 2020.14.1:" part1)
  #_(println "Advent of Code 2020.14.2:" part2))
