(ns aoc.2024.20
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [ubergraph.alg :as alg]
            [medley.core :as m]
            [ubergraph.core :as uber]
            [clojure.test :refer [deftest is testing]]
            [pathfinding :as pf]
            [pp-grid.api :as pp]))

;; # Parser
(defn parser [data]
  (->> data
       u/to-matrix
       pf/decode-matrix))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/20.txt"))
                parser))

(def input-example (parser "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"))

;; Logic
(defn neighbors [cell grid]
  (let [[y x] cell]
    (filter #(and (grid %))
            [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])))

(defn shortest-path [path-cells & {:keys [start end max-cost] :or {max-cost 100000}}]
  (let [edges (mapcat (fn [[from]]
                        (map (fn [to] [from to]) (neighbors from path-cells))) path-cells)
        g (-> (uber/graph)
              (uber/add-edges* edges))
        total-cost (atom 0)]
    (alg/shortest-path g {:start-node start
                          :end-node end
                          :cost-fn (fn [_]
                                     (if (> @total-cost max-cost)
                                       (throw (Exception. "Max cost"))
                                       (do (swap! total-cost inc)
                                           1)))})
    @total-cost))

;; ## Part 1
(defn part-1
  [{:keys [cells width height]}]
  (let [start (ffirst (m/filter-vals (fn [v] (= v \S)) cells))
        end (ffirst (m/filter-vals (fn [v] (= v \E)) cells))
        cells (assoc cells start \. end \.)
        path-cells (m/filter-vals (fn [v] (= v \.)) cells)
        full-cost (shortest-path path-cells :start start :end end)
        wall-cells (filter
                    (fn [[[y x] v]]
                      (and (= v \#)
                           (not= y 0)
                           (not= x 0)
                           (not= y (dec height))
                           (not= x (dec width))))
                    cells)]
    (->> wall-cells
         (reduce
          (fn [total [wall-coord]]
            (let [cells (assoc path-cells wall-coord \.)]
              (if-let [cost (try (shortest-path cells :start start :end end :max-cost 100)
                                 (catch Exception e
                                   nil))]
                (inc total)
                total)))
          0))))


;; ## Part 2
(defn part-2
  [{:keys [cells width height]}]
  (let [start (ffirst (m/filter-vals (fn [v] (= v \S)) cells))
        end (ffirst (m/filter-vals (fn [v] (= v \E)) cells))
        cells (assoc cells start \. end \.)
        path-cells (m/filter-vals (fn [v] (= v \.)) cells)
        full-cost (shortest-path path-cells :start start :end end)
        wall-cells (filter
                    (fn [[[y x] v]]
                      (and (= v \#)
                           (not= y 0)
                           (not= x 0)
                           (not= y (dec height))
                           (not= x (dec width))))
                    cells)]
    (->> (for [[wall-coord] wall-cells
               :let [cells (assoc path-cells wall-coord \.)
                     cost (shortest-path cells :start start :end end)]
               :when (< cost full-cost)
               :let [diff (- full-cost cost)]
               :when (>= diff 0)]
           diff))))

;; # Tests
#_{:nextjournal.clerk/visibility {:code   :show
                                  :result :hide}}
#_(deftest test-2024-20
    #_(testing "part one"
        (is (= 1507 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

