(ns advent-clj.2020.day04
  (:require [advent-clj.core :refer [get-puzzle-input]]
            [clojure.set :refer [subset?]]
            [clojure.string :as s])
  (:gen-class))

(def input (get-puzzle-input "2020/day04.txt"))

(defn into-hashmap [x]
  (apply hash-map (s/split x #"( |\:)")))

(def make-passport
  (comp into-hashmap #(s/join " " %)))

(def passports
  (->> input
       (partition-by empty?)
       (remove #(= '("") %))
       (map make-passport)))

(defn valid? [passport]
  (subset? #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}
           (into #{} (keys passport))))

(defn valid-ecl? [x] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} x))
(defn valid-byr? [x] (contains? (set (map str (range 1920 2003))) x))
(defn valid-iyr? [x] (contains? (set (map str (range 2010 2021))) x))
(defn valid-eyr? [x] (contains? (set (map str (range 2020 2031))) x))
(defn valid-hcl? [x] (some? (re-matches #"^#[0-9a-f]{6}$" x)))
(defn valid-pid? [x] (some? (re-matches #"^[0-9]{9}$" x)))
(defn valid-hgt? [x]
  (let [[_ len unit] (re-matches #"^([0-9]{2,3})(in|cm)$" x)
        valid-cm (set (map str (range 150 194)))
        valid-in (set (map str (range 59 77)))]
    (cond (and (= "cm" unit) (contains? valid-cm len)) true
          (and (= "in" unit) (contains? valid-in len)) true
          :else false)))

(defn valid?? [passport]
  (and (valid? passport)
       (valid-ecl? (get passport "ecl"))
       (valid-byr? (get passport "byr"))
       (valid-iyr? (get passport "iyr"))
       (valid-eyr? (get passport "eyr"))
       (valid-hcl? (get passport "hcl"))
       (valid-pid? (get passport "pid"))
       (valid-hgt? (get passport "hgt"))))

(def part1 (->> passports (filter valid?) count))
(def part2 (->> passports (filter valid??) count))

(defn -main []
  (println "Advent of Code 2020-04.1:" part1)
  (println "Advent of Code 2020-04.2:" part2))
