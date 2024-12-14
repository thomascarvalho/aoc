^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.18
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [pathfinding :as pf]
            [clojure.test :refer :all]
            [pp-grid.api :as pg]))

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-matrix
       pf/decode-matrix))

(def input (->> (slurp (io/resource "inputs/2015/18.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

(defn create-grid [cells]
  (let [assoc-data (fn [grid cells] (reduce (fn [acc [coords c]] (assoc acc (into [] (reverse coords)) c)) grid cells))]
    (-> (pg/empty-grid)
        (assoc-data (into [] cells)))))

;;  Example
(def input-example (parser ".#.#.#
...##.
#....#
..#...
#.#..#
####.."))

(defn neighbors-values [cell grid]
  (let [[y x] cell]
    (for [n     [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]
                 [(dec y) (dec x)] [(dec y) (inc x)]
                 [(inc y) (dec x)] [(inc y) (inc x)]]
          :let  [v (grid n)]
          :when v]
      v)))

;; ## Part 1
(defn part-1
  [{:keys [cells]}]
  (loop [cells cells
         step  0]

    (if (= step 100)
      (-> cells
          vals
          frequencies
          (get \#))
      (recur (reduce-kv
              (fn [r coords v]
                (let [n-freqs (frequencies (neighbors-values coords cells))
                      n-on    (or (get n-freqs \#) 0)]
                  (cond-> r
                    (= v \.) (assoc coords (if (= n-on 3) \# \.))
                    (= v \#) (assoc coords (if (<= 2 n-on 3) \# \.))))) {} cells) (inc step)))))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [{:keys [width height cells]}]
  (let [fixed-cells [[0 0] [0 (dec width)]
                     [(dec height) 0] [(dec height) (dec width)]]]
    (loop [cells (reduce (fn [r coords]
                           (assoc r coords \#)) cells fixed-cells)
           step  0]
      (if (= step 100)
        (-> cells
            vals
            frequencies
            (get \#))
        (recur
         (reduce-kv
          (fn [r coords v]
            (if (some #{coords} fixed-cells)
              (assoc r coords \#)
              (let [n-freqs (frequencies (neighbors-values coords cells))
                    n-on    (or (get n-freqs \#) 0)]
                (cond-> r
                  (= v \.) (assoc coords (if (= n-on 3) \# \.))
                  (= v \#) (assoc coords (if (<= 2 n-on 3) \# \.)))))) {} cells)
         (inc step))))))



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-18
  (testing "part one"
    (is (= 814 (part-1 input))))

  (testing "part two"
    (is (= 924 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
