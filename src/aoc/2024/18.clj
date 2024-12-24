(ns aoc.2024.18
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [pp-grid.api :as pp]
            [ubergraph.alg :as alg]
            [ubergraph.core :as uber]
            [pathfinding :as pf]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [bits-count start end data]
  {:lines (->> data u/to-lines)
   :start start
   :end end
   :bits-count bits-count})

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/18.txt"))
                (parser 1024 [0 0] [70 70])))

(def input-example (parser 12 [0 0] [6 6] "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"))

;; Logic


(defn keep-bits [bits-count lines]
  (let [bits (->> lines
                  (take bits-count))]
    {:last-byte (last bits)
     :cells (->> bits
                 (mapv (fn [line]
                         (let [coord (into [] (u/parse-out-longs line))]
                           (hash-map coord \#))))
                 (apply merge))}))


(defn get-empty-cells [cells [start-x start-y] [end-x end-y]]
  (let [empty-cells (for [x (range start-x (inc end-x))
                          y (range start-y (inc end-y))
                          :let [coords [x y]]
                          :when (not (cells coords))]
                      coords)]
    {:empty-cells (set empty-cells)
     :grid (->> empty-cells
                (reduce
                 (fn [grid empty-coords]
                   (assoc grid empty-coords \.))
                 cells))}))

(defn neighbors [cell empty-cells]
  (let [[y x] cell]
    (filter #(empty-cells %)
            [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])))

(defn create-edges [empty-cells]
  (mapcat (fn [from]
            (mapv (fn [to]
                    [from to]) (neighbors from empty-cells))) empty-cells))

;; ## Part 1
(defn part-1
  [{:keys [lines bits-count start end]}]
  (let [{:keys [cells]} (keep-bits bits-count lines)
        {:keys [empty-cells _grid]} (get-empty-cells cells start end)
        edges (create-edges empty-cells)
        g (-> (uber/graph)
              (uber/add-edges* edges))]
    (-> g
        (alg/shortest-path start end)
        :cost)))

;; ## Part 2

(defn part-2
  [{:keys [lines bits-count start end]}]
  (loop [bits-count (inc bits-count)]
    (let [{:keys [cells last-byte]} (keep-bits bits-count lines)
          {:keys [empty-cells _grid]} (get-empty-cells cells start end)
          edges (create-edges empty-cells)
          g (-> (uber/graph)
                (uber/add-edges* edges))]
     (if (-> g
            (alg/shortest-path start end)
            :cost)
       (recur (inc bits-count))
       last-byte))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-18
  (testing "part one"
    (is (= 22 (part-1 input-example)))
    (is (= 290 (part-1 input))))

  (testing "part two"
    (is (= "64,54" (part-2 input)))))

