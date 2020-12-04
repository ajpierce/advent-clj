(ns advent-clj.2020.day04
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.set :refer [subset?]]
            [clojure.string :as s])
  (:gen-class))

(def input (get-puzzle-input "2020/day04.txt"))

(defn into-hashmap [x]
  (apply hash-map (s/split x #"( |\:)")))
(def make-passport (comp into-hashmap #(s/join " " %)))
(def passports (->> input
                    (partition-by empty?)
                    (remove #(= '("") %))
                    (map make-passport)))

(def rules {"ecl" #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %)
            "byr" #(contains? (set (map str (range 1920 2003))) %)
            "iyr" #(contains? (set (map str (range 2010 2021))) %)
            "eyr" #(contains? (set (map str (range 2020 2031))) %)
            "hcl" #(some? (re-matches #"^#[0-9a-f]{6}$" %))
            "pid" #(some? (re-matches #"^[0-9]{9}$" %))
            "hgt" #(let [[_ len unit] (re-matches #"^([0-9]{2,3})(in|cm)$" %)
                         valid-cm (set (map str (range 150 194)))
                         valid-in (set (map str (range 59 77)))]
                     (cond (and (= "cm" unit) (contains? valid-cm len)) true
                           (and (= "in" unit) (contains? valid-in len)) true
                           :else false))})

(defn valid? [passport]
  (subset? (into #{} (keys rules))
           (into #{} (keys passport))))

(defn valid?? [passport]
  (and (valid? passport)
       (every? (fn [[k v]] ((get rules k identity) v)) passport)))

(def part1 (->> passports (filter valid?) count))
(def part2 (->> passports (filter valid??) count))

(defn -main []
  (println "Advent of Code 2020-04.1:" part1)
  (println "Advent of Code 2020-04.2:" part2))
