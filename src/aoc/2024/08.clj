(ns aoc.2024.08
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [util :as u]
            [clojure.math.combinatorics :as combo]
            [pathfinding :as pf]
            [medley.core :as m]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (->> data
       u/to-matrix
       pf/decode-matrix))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/08.txt"))
                parser))

(def input-example (parser "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"))

;; Logic

(defn group-cells-by-antenas [cells]
  (->> (group-by val cells)
       (m/filter-keys (fn [k] (not= k \.)))
       (m/map-vals (fn [coords]
                     (mapv (fn [[coord]] coord) coords)))))

(defn extend-segment
  "Retourne [point-avant point-après] qui étendent le segment"
  [[y1 x1] [y2 x2] point-in-grid?]
  (let [dy (- y2 y1)
        dx (- x2 x1)]
    (filter point-in-grid? [[(- y1 dy) (- x1 dx)]
                            [(+ y2 dy) (+ x2 dx)]])))

(defn create-antinodes-by-antenas-segments [create-fn grouped-antennas]
  (->> grouped-antennas
       (reduce
        (fn [antinodes [_ [& coords]]]
          (->> (mapcat (fn [[c1 c2]]
                         (create-fn c1 c2)) (combo/combinations coords 2))
               (apply conj antinodes)))
        #{})))

;; ## Part 1
(defn part-1
  [data]
  (let [{:keys [cells]} data]
    (->> cells
         group-cells-by-antenas
         (create-antinodes-by-antenas-segments
          (fn [p1 p2] (extend-segment p1 p2 #(get cells %))))
         count)))

;; ## Part 2
(defn extend-segment-to-grid
  "Retourne tous les points qui étendent le segment dans les deux directions
   jusqu'à sortir de la grille"
  [[y1 x1 :as p1] [y2 x2 :as p2] point-in-grid?]
  (let [dy (- y2 y1)
        dx (- x2 x1)
        gen-points (fn [p-start step-y step-x]
                     (->> (iterate (fn [[y x]] [(+ y step-y) (+ x step-x)])
                                   p-start)
                          (take-while #(point-in-grid? %))))
        points-before (gen-points p1 (- dy) (- dx))
        points-after (gen-points p2 dy dx)]
    (concat points-before points-after)))

(defn part-2
  [data]
  (let [{:keys [cells]} data]
    (->> cells
         group-cells-by-antenas
         (create-antinodes-by-antenas-segments
          (fn [p1 p2]
            (extend-segment-to-grid p1 p2 #(get cells %))))
         count)))


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-08
  (testing "part one"
    (is (= 14 (part-1 input-example)))
    (is (= 220 (part-1 input))))

  (testing "part two"
    (is (= 34 (part-2 input-example)))
    (is (= 813 (part-2 input)))))
