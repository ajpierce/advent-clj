(ns advent-clj.day16
  (:require [advent-clj.core :refer [get-partitioned-input]]
            [clojure.set :refer [intersection difference]]
            [clojure.string :as s]))

(def input (get-partitioned-input "2020/day16.txt"))
(def samp (get-partitioned-input "2020/day16-sample.txt"))

(def rule-regex #"^(.*): (\d+)-(\d+) or (\d+)-(\d+)$")
(defn parse-rule [^String rule]
  (let [[_ nom x1 x2 y1 y2] (re-matches rule-regex rule)
        range1 (range (Integer/parseInt x1) (inc (Integer/parseInt x2)))
        range2 (range (Integer/parseInt y1) (inc (Integer/parseInt y2)))
        values (set (concat range1 range2))
        f (partial contains? values)]
    {:name nom :valid? f :values values}))

(def rules (map parse-rule (first input)))

(defn rule-reducer [in acc rule]
  (if ((:valid? rule) in)
    (conj acc (:name rule))
    acc))

(defn parse-ticket [^String ticket]
  (->> (s/split ticket #",")
       (map #(Integer/parseInt %))
       (map-indexed
        #(hash-map :position %1
                   :value %2
                   :satisfies (reduce (partial rule-reducer %2) #{} rules)))))

(defn fields-reducer [acc field]
  (assoc acc (:position field) (:satisfies field)))

(defn valid-field-reducer [acc [pos value]]
  (update acc pos (fn [x] (let [ix (intersection value x)]
                            (cond (nil? x) value
                                  (empty? ix) x
                                  :else ix)))))

(defn ticket-reducer [acc ticket]
  (let [fields (reduce fields-reducer (sorted-map) ticket)]
    (reduce valid-field-reducer acc fields)))

(def possibilities
  (let [my-ticket (last (second input))
        other-tickets (rest (last input))]
    (->> (conj other-tickets my-ticket)
         (map parse-ticket)
         (reduce ticket-reducer (sorted-map))
         (sort-by (comp count second)))))

(def validators (reduce #(assoc %1 (:name %2) (:valid? %2)) {} rules))

(def indexed-validators
  (loop [vs {} seen #{} remaining possibilities]
    (if (empty? remaining) vs
        (let [[idx poss] (first remaining)
              field (first (difference poss seen))
              validator (assoc (first (filter #(= field (:name %)) rules)) :idx idx)]
          (recur (assoc vs idx validator) (conj seen field) (rest remaining))))))

(defn keep-invalid [^String ticket]
  (->> (s/split ticket #",")
       (map #(Integer/parseInt %))
       (map-indexed #(if ((:valid? (get indexed-validators %1)) %2) nil %2))
       (remove nil?)))

(def part1
  (->> input last rest
       (map keep-invalid)
       flatten
       (reduce +)))

(def departure-fields
  (->> indexed-validators
       (filter (fn [[k v]] (s/includes? (:name v) "departure")))
       keys set))

(defn keep-departures [^String ticket]
  (->> (s/split ticket #",")
       (map #(Integer/parseInt %))
       (map-indexed #(if (contains? departure-fields %1) %2 nil))
       (remove nil?)))

(def part2
  (->> input second rest first
       keep-departures
       (reduce *)))
