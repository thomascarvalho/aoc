(ns aoc.2024.16
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [medley.core :as m]
            [clojure.string :as str]
            [clojure.set :as cset]
            [clojure.test :refer [deftest is testing]]
            [pathfinding :as pf]
            [ubergraph.alg :as alg]
            [ubergraph.core :as uber])
  (:import [java.util PriorityQueue]))

;; # Parser
(defn parser [data]
  (->> data
       u/to-matrix
       (pf/decode-matrix)))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/16.txt"))
                parser))

(def input-example (parser "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"))

;; Logic

(defn get-offset [[from-y from-x] [to-y to-x]]
  [(- to-y from-y)
   (- to-x from-x)])

(defn neighbors [cell grid]
  (let [[y x] cell]
    (filter #(and (grid %) (not= (grid %) \#))
            [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])))


(defn get-neighbors [edges node]
  (->> edges
       (filter #(= (first %) node))
       (map second)
       set))

(defn get-direction [[y1 x1] [y2 x2]]
  (cond
    (< y2 y1) :up
    (> y2 y1) :down
    (< x2 x1) :left
    (> x2 x1) :right))

(defn dijkstra-p1 [edges start-node end-node]
  (let [queue (java.util.PriorityQueue.
               (fn [[_ _ cost1] [_ _ cost2]]
                 (compare cost1 cost2)))
        seen (atom {})]

    ;; Initialisation avec direction droite
    (.offer queue [start-node :right 0])
    (swap! seen assoc [start-node :right] [0 [start-node]])

    (loop []
      (if-let [[node dir cost] (.poll queue)]
        (if (= node end-node)
          (let [[final-cost final-path] (@seen [node dir])]
            [final-cost final-path])
          (do
            (doseq [next-node (get-neighbors edges node)]
              (let [next-dir (get-direction node next-node)
                    next-cost (+ cost (if (= dir next-dir) 1 1001))
                    [prev-cost _] (get @seen [next-node next-dir] [Integer/MAX_VALUE nil])
                    current-path (second (@seen [node dir]))]

                (when (< next-cost prev-cost)
                  (swap! seen assoc [next-node next-dir] [next-cost (conj current-path next-node)])
                  (.offer queue [next-node next-dir next-cost]))))
            (recur)))
        nil))))

;; ## Part 1
(defn part-1
  [{:keys [cells]}]
  (let [start (ffirst (m/filter-vals (fn [v] (= v \S)) cells))
        end (ffirst (m/filter-vals (fn [v] (= v \E)) cells))
        edges (mapcat
               (fn [[from value]]
                 (when (not= value \#)
                   (mapv (fn [to]
                           [from to])
                         (neighbors from cells))))
               cells)]
    (first (dijkstra-p1 edges start end))))

;; ## Part 2

(def base-move-score 1)
(def turn-move-score 1001)

(defn dijkstra-all-optimal [edges start-node end-node & {:keys [max-best-paths] :or {max-best-paths 1}}]
  (let [queue (PriorityQueue. 
               (fn [[_ _ cost1 _] [_ _ cost2 _]]
                 (compare cost1 cost2)))
        paths-found (atom #{})
        min-cost (atom Integer/MAX_VALUE)]
    
    ;; [node direction cost path]
    (.offer queue [start-node :right 0 [start-node]])
    
    (while (and (not (.isEmpty queue))
                (< (count @paths-found) max-best-paths))
      (let [[node dir cost path] (.poll queue)]
        (if (= node end-node)
          (when (<= cost @min-cost)
            (reset! min-cost cost)
            (swap! paths-found conj (set path)))
          
          (doseq [next-node (get-neighbors edges node)]
            (let [next-dir (get-direction node next-node)
                  next-cost (+ cost (if (= dir next-dir) base-move-score turn-move-score))
                  next-path (conj path next-node)]
              
              ;; Continue seulement si ce chemin peut mener à une solution optimale
              (when (<= next-cost @min-cost)
                (.offer queue [next-node next-dir next-cost next-path])))))))
    
    [@min-cost @paths-found]))

(defn part-2
  [{:keys [cells]}]
  (let [start (ffirst (m/filter-vals (fn [v] (= v \S)) cells))
        end (ffirst (m/filter-vals (fn [v] (= v \E)) cells))
        edges (mapcat
               (fn [[from value]]
                 (when (not= value \#)
                   (mapv (fn [to]
                           [from to])
                         (neighbors from cells))))
               cells)
        [_ paths] (dijkstra-all-optimal edges start end :max-best-paths 2)]
       (count (apply cset/union paths))))

(time (part-2 input))


;; # Tests
#_{:nextjournal.clerk/visibility {:code   :show
                                  :result :hide}}
#_(deftest test-2024-16
    #_(testing "part one"
        (is (= 115500 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

