(ns aoc.2025.04
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [pathfinding :as pf]
            [clojure.set :as cset]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/04.txt"))))

(def input-example "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(defn parser [input]
  (->> input
       str/split-lines
       pf/decode-matrix
       :cells
       (filter (fn [[_k c]] (= c \@)))
       (map first)
       set))

(def neighbors-offsets [[-1 0] [1 0] [0 -1] [0 1] [-1 -1] [1 1] [-1 1] [1 -1]])

(defn get-neighbors-papers [cell all-papers]
  (for [offset neighbors-offsets
        :let [n (mapv + cell offset)]
        :when (all-papers n)]
    n))

(defn part-one
  [data]
  (let [papers (parser data)]
    (->> papers
         (reduce
          (fn [acc coords]
            (let [neighbors-papers (get-neighbors-papers coords papers)]
              (if (< (count neighbors-papers) 4)
                (conj acc coords)
                acc)))
          #{})
         count)))

(defn part-two
  [data]
  (loop [papers (parser data)
         removed-count 0
         step 0]
    (if (> step 1000)
      (format "Too many steps %s" step)
      (let [papers-to-remove (->> papers
                                  (reduce
                                   (fn [acc coords]
                                     (let [neighbors-papers (get-neighbors-papers coords papers)]
                                       (if (< (count neighbors-papers) 4)
                                         (conj acc coords)
                                         acc)))
                                   #{}))]
        (if (seq papers-to-remove)
          (recur
           (cset/difference papers papers-to-remove)
           (+ removed-count (count papers-to-remove))
           (inc step))
          removed-count)))))

(deftest test-2025-04
  (testing "part one"
    (is (= 13 (part-one input-example)))
    (is (= 1449 (part-one input))))

  (testing "part two"
    (is (= 43 (part-two input-example)))
    (is (= 8746 (part-two input)))))

