(ns aoc.2024.10
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [util :as u]
            [clojure.test :refer [deftest is testing]]
            [ubergraph.alg :as alg]
            [ubergraph.core :as uber]
            [pathfinding :as pf]))

;; # Parser
(defn parser [data]
  (->> data
       u/to-matrix
       (pf/decode-matrix (comp parse-long str))))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/10.txt"))
                parser))

(def input-example (parser "0123
1234
8765
9876"))

(def input-example-2 (parser "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"))

;; Logic

;; ## Part 1
(defn part-1
  [data]
  (let [{:keys [cells]} data
        edges (mapcat (fn [[coords v]]
                        (->> (pf/neighbors coords cells)
                             (mapv (fn [target]
                                     (let [v2 (get cells target)]
                                       [[coords v] [target v2] (- v2 v)])))
                             (filter (fn [[_ _ h]]
                                       (= h 1)))))
                      cells)
        g (-> (uber/digraph)
              (uber/add-edges* edges))
        start-nodes (filter (fn [[_ v]]
                              (= v 0)) cells)
        end-nodes (filter (fn [[_ v]]
                            (= v 9)) cells)]
    (->> (for [start start-nodes
               end end-nodes
               :let [path (alg/shortest-path g start end)]
               :when path]
           (deref (:list-of-edges path)))
         (into #{})
         count)))

;; ## Part 2
(defn part-2
  [data]
  (let [{:keys [cells]} data
        edges (mapcat (fn [[coords v]]
                        (->> (pf/neighbors coords cells)
                             (mapv (fn [target]
                                     (let [v2 (get cells target)]
                                       [[coords v] [target v2] (- v2 v)])))
                             (filter (fn [[_ _ h]]
                                       (= h 1)))))
                      cells)
        g (-> (uber/digraph)
              (uber/add-edges* edges))
        start-nodes (filter (fn [[_ v]]
                              (= v 0)) cells)
        end-nodes (filter (fn [[_ v]]
                            (= v 9)) cells)]
    (->> (for [start start-nodes
               end end-nodes]
           (count (pf/find-all-paths g start end)))
         (reduce +))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-10
  (testing "part one - example 2"
    (is (= 36 (part-1 input-example-2))))
  (testing "part one"
    (is (= 659 (part-1 input))))
  (testing "part two"
    (is (= 1463 (part-2 input)))))











