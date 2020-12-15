(ns advent-clj.2020.day15)

(def input [6 13 1 15 2 0])

(defn solve [starting-nums until]
  (loop [iteration (inc (count starting-nums))
         last-spoken (last starting-nums)
         acc (reduce-kv #(assoc %1 %3 (list (inc %2) nil)) {} starting-nums)]
    (let [last-spoke-turns (get acc last-spoken (list nil nil))
          spoken (if (some nil? last-spoke-turns) 0
                     (apply - last-spoke-turns))]
      (if (= until iteration) spoken
          (recur (inc iteration) spoken
                 (update acc spoken #(cons iteration (if (nil? %)
                                                       (list nil)
                                                       (butlast %)))))))))

(defn -main []
  (println "Advent of Code 2020.15.1:" (time (solve input 2020)))
  (println "Advent of Code 2020.15.2:" (time (solve input 30000000))))
