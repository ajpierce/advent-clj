(ns advent-clj.2020.day07
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.string :as s]))

(def input (get-puzzle-input "2020/day07.txt"))

(defn parse-bag [^String bag]
  (let [[_ qty color] (re-matches #"^(\d{1,2})? (.*)$" bag)]
    [color qty]))

(def clean (comp #(map s/trim %)
                 #(s/split % #"( contain |,)")
                 #(s/replace % #"(bag(s?)|\.)" "")))

(defn just-bags-reducer [acc [bag & contents]]
  (assoc acc bag (set (map (comp first parse-bag) contents))))

(def just-bags (->> input (map clean) (reduce just-bags-reducer {})))

(defn get-containers [^String bag]
  (->> just-bags
       (filter (fn [[k v]] (contains? v bag)))
       keys))

(def part1
  (loop [eligible #{} to-process ["shiny gold"]]
    (if (empty? to-process) (count eligible)
        (let [containers (get-containers (first to-process))]
          (recur (apply conj eligible containers)
                 (concat (rest to-process) containers))))))

(comment

  (defn quantified-bags-reducer [acc [bag & contents]]
    (assoc acc bag (map parse-bag contents)))

  (def quantified-bags (->> input (map clean-qtys) (reduce quantified-bags-reducer {})))

  (clojure.pprint/pprint quantified-bags)

  (get quantified-bags "shiny gold")

  (def part2 (loop [num-bags [] to-process ["shiny gold"]]
               (if (empty? to-process) (reduce + num-bags)
                   (let [contents (get quantified-bags (first to-process))
                         bags (map first contents)
                         qtys (map (comp #(try (Integer/parseInt %) (catch Exception e 1)) last) contents)]
                     (println "contents" contents "bags" bags "qtys" qtys)
                     (recur (conj num-bags (reduce + 1 qtys)) (concat (rest to-process) bags)))))))
