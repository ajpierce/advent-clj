(ns advent-clj.2020.day11
  (:require [advent-clj.core :refer [get-puzzle-input]]))

(defn parse [^String x]
  (->> x seq (map #(if (= \L %) :empty nil)) vec))

(def input (->> (get-puzzle-input "2020/day11.txt") (map parse) vec))

(defn surrounding
  "Calculate surrounding coordinates, including where there are no chairs"
  [_ [x y]]
  (for [dx [-1 0 1]
        dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(defn next-occupied
  "Returns coords of the nearest chair for the given puzzle, [y x] coord pair, and direction.
  Returns corrds at the puzzle boundary if no chairs encountered in the given direction."
  [seats loc direction]
  (let [maxx (count (first seats))
        maxy (count seats)
        [y x] loc]
    (loop [scale 1]
      (let [[dy dx] (map (partial * scale) direction)
            newx (+ x dx)
            newy (+ y dy)
            new-loc [newy newx]
            seat (get-in seats new-loc)]
        (if (or (some? seat) (neg? newx) (neg? newy) (>= newx maxx) (>= newy maxy))
          new-loc
          (recur (inc scale)))))))

(defn nearest-chairs
  "Calculate nearest neighbors, following empty space to nearest chair"
  [seats loc]
  (for [dx [-1 0 1]
        dy (if (zero? dx) [-1 1] [-1 0 1])]
    (next-occupied seats loc [dx dy])))

(defn count-occupied
  "Use the provided get-neighbors function to identify number of occupied seats"
  ([seats loc] (count-occupied surrounding seats loc))  ;; use surrounding as default get-neighbors fn
  ([get-neighbors seats loc]
   (let [n (map #(get-in seats %) (get-neighbors seats loc))]
     (count (filter #(= % :occupied) n)))))

(defn sit-or-split
  "Determine state change for a given location.
  Optionally accepts count-fn and seat depature threshold"
  ([seats loc] (sit-or-split count-occupied 4 seats loc))
  ([n seats loc] (sit-or-split count-occupied n seats loc))
  ([count-fn n seats loc]
   (let [current-seat       (get-in seats loc)
         occupied-neighbors (count-fn seats loc)]
     (cond (and (= :empty current-seat) (= 0 occupied-neighbors))      :occupied
           (and (= :occupied current-seat) (>= occupied-neighbors n))  :empty
           :else current-seat))))

(defn step
  ([seats] (step sit-or-split seats))
  ([f seats]
   (into [] (for [i (range (count seats))]
              (into [] (for [j (range (count (get seats i)))]
                         (f seats [i j])))))))

(defn solve [stepper i]
  (let [prev-state (atom nil)]
    (->> (iterate stepper i)
         (take-while (fn [x] (if (not= @prev-state x)
                               (do (reset! prev-state x) true)
                               false)))
         (last) (flatten)
         (filter #(= % :occupied))
         (count))))

(def part2-counter (partial count-occupied nearest-chairs))
(def part2-decider (partial sit-or-split part2-counter 5))
(def part2-stepper (partial step part2-decider))

(defn -main []
  (println "Advent of Code 2020.11.1:" (time (solve step input)))
  (println "Advent of Code 2020.11.2:" (time (solve part2-stepper input))))
