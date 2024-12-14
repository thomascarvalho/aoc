(ns aoc.2024.12
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [pathfinding :as pf]
            [clojure.test :refer [deftest is testing]]
            [medley.core :as m]))

;; # Parser
(defn parser [data]
  (->> data
       u/to-matrix
       pf/decode-matrix))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/12.txt"))
                parser))

(def input-example (parser "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"))

;; Logic
(defn neighbors [[cell v] grid]
  (let [[y x] cell]
    (filter #(= (grid %) v)
            [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])))

(defn find-by-neighbours [source-coords others]
  (filter (fn [other-coords]
            (seq (filter (fn [other-coord]
                           (some #(= % other-coord) source-coords))
                         other-coords)))
          others))

(defn count-perimeter
  "Calcule le périmètre en comptant les côtés externes"
  [points]
  (let [points-set points
        min-x (apply min (map first points))
        max-x (apply max (map first points))
        min-y (apply min (map second points))
        max-y (apply max (map second points))
        total-sides (* 4 (count points))
        shared-sides (reduce +
                             (for [x (range min-x (inc max-x))
                                   y (range min-y (inc max-y))
                                   :when (points-set [x y])]
                               (+ (if (points-set [x (inc y)]) 2 0)      ;; voisin du haut
                                  (if (points-set [(inc x) y]) 2 0))))]  ;; voisin de droite
    (- total-sides shared-sides)))






;; ## Part 1
(defn part-1
  [data]
  (let [{:keys [cells]} data
        indexed-neighbors (->> (for [[coords v :as c] cells]
                                 (into #{coords} (neighbors c cells))))
        regions (loop [[mini-group & others] indexed-neighbors
                       regions []]
                  (if mini-group
                    (let [[current-region others'] (loop [region mini-group
                                                          others others]
                                                     (if-let [targets (seq (find-by-neighbours region others))]
                                                       (let [others' (remove #(some (fn [r] (= % r)) targets) others)]
                                                         (recur (into region (sequence cat targets))
                                                                others'))
                                                       [region others]))]
                      (recur others'
                             (conj regions current-region)))
                    regions))]
    (->> regions
         (mapv (fn [points]
                 (* (count points) (count-perimeter points))))
         (reduce +))))

;; ## Part 2

(defn count-sides-direction [points pred-coords]
  (->> points
       (reduce (fn [t [_ targets]]
                 (if-let [segments (seq (->> (sort targets)
                                             (filter (fn [coords]
                                                       (nil? (pred-coords coords))))))]
                   (let [ratio (double (/ 1 (count segments)))]
                     (+ t (* ratio (count segments))))
                   t))
               0)))

(defn count-sides [points]
  (let [same-x-points (group-by second points)
        same-y-points (group-by first points)]
    (->> [(count-sides-direction same-x-points (fn [[y x]]
                                                 (points [y (dec x)])))
          (count-sides-direction same-x-points (fn [[y x]]
                                                 (points [y (inc x)])))
          (count-sides-direction same-y-points (fn [[y x]]
                                                 (points [(dec y) x])))
          (count-sides-direction same-y-points (fn [[y x]]
                                                 (points [(inc y) x])))]
         (reduce +)
         )))



(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
#_(deftest test-2024-12
    #_(testing "part one"
        (is (= 1415378 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

(time
 (let [data input-example]
   (let [{:keys [cells]} data
         indexed-neighbors (->> (for [[coords v :as c] cells]
                                  (into #{coords} (neighbors c cells))))
         regions (loop [[mini-group & others] indexed-neighbors
                        regions []]
                   (if mini-group
                     (let [[current-region others'] (loop [region mini-group
                                                           others others]
                                                      (if-let [targets (seq (find-by-neighbours region others))]
                                                        (let [others' (remove #(some (fn [r] (= % r)) targets) others)]
                                                          (recur (into region (sequence cat targets))
                                                                 others'))
                                                        [region others]))]
                       (recur others'
                              (conj regions current-region)))
                     regions))]
     (->> regions
          (mapv (fn [points]
                   [ (count points) (count-sides points)]))
          #_(reduce +)))))



