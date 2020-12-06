(ns advent-clj.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn get-puzzle-input
  "Given the name of a file in the resources folder,
  parse it and return a vec of the lines in the file.
  If the file is a single line, return a string instead of a vec"
  [^String filename]
  (let [lines (-> (io/resource filename)
                  slurp
                  clojure.string/split-lines)]
    (if (= 1 (count lines))
      (first lines)
      lines)))

(defn get-partitioned-input
  "Return puzzle input as groups partitioned by \n\n"
  [^String filename]
  (let [lines (-> (io/resource filename)
                  slurp
                  clojure.string/split-lines)]
    (->> lines
         (partition-by empty?)
         (remove #(= '("") %)))))
