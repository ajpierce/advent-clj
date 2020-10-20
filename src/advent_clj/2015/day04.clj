(ns advent-clj.2015.day04
  (:require [clojure.core.reducers :as r])
  (:gen-class))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(def input "yzbqklnj")

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn mined? [^String h] (= "00000" (subs h 0 5)))
(defn mined?? [^String h] (= "000000" (subs h 0 6)))

(def hash-it (comp md5 #(str input %)))

(def correct? (comp mined? hash-it))
(def correct?? (comp mined?? hash-it))

(comment "The original solutions ended up being pretty slow"

(def part1
  (time
  (loop [n 0]
    (if (mined? (md5 (str input n)))
      n
      (recur (inc n))))))

(def part2
  (time
  (loop [n 0]
    (if (mined?? (md5 (str input n)))
      n
      (recur (inc n))))))
)

; We can use all the cores in parallel using reducers
(def part1-reducers
  (time
   (->> (range 300000)
        vec
        (r/filter correct?)
        (r/foldcat)
        min first)))

(def part2-reducers
  (time
   (->> (range 10000000)
        vec
        (r/filter correct??)
        (r/foldcat)
        min first)))

(defn -main []
  (println "2015 Day 04, Part 1: " part1-reducers)
  (println "2015 Day 04, Part 2: " part2-reducers))

; On a 2013 13" Macbook Pro:
;; Before:
; "Elapsed time: 3373.290997 msecs"
; "Elapsed time: 107959.537249 msecs
;
;; After:
; "Elapsed time: 1788.525751 msecs"
; "Elapsed time: 53510.56768 msecs"
;
; About a 2x speedup using reducers to solve the problem
