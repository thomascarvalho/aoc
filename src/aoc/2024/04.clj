(ns aoc.2024.04
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.core.matrix :as ma]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
;; # Solution
(def input (slurp (io/resource "inputs/2024/04.txt")))

;;  Example
(def input-example "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

;; ## Part 1
(defn part-1
  [data]
  (let [m (u/to-matrix data)
        m-inversed  (mapv #(into [] (reverse %)) (u/to-lines data))
        x-max (ma/column-count m)
        x-indexes (range (- x-max) x-max)
        rows (ma/rows m)
        columns (ma/columns m)
        diagonals (mapcat (fn [x]
                            [(ma/diagonal m x)
                             (ma/diagonal m-inversed x)]) x-indexes)]
    (->> (concat
          (map str/join diagonals)
          (map str/join rows)
          (map str/join columns))
         (reduce
          (fn [total s]
            (+ total (count (re-seq #"XMAS" s)) (count (re-seq #"SAMX" s))))
          0))))

;; ## Part 2
(defn part-2
  [data]
  (let [m (u/to-matrix data)
        cells (apply merge (flatten (ma/emap-indexed (fn [coords v] (hash-map (into [] coords) v)) m)))
        x-mas? (fn [v] (some #(= % v) [[\M \A \S] [\S \A \M]]))]
    (->> cells
         (filter (fn [[_ v]] (= v \A)))
         (reduce
          (fn [total [[y x]]]
            (let [top-left (get cells [(dec y) (dec x)])
                  top-right (get cells [(dec y) (inc x)])
                  bottom-left (get cells [(inc y) (dec x)])
                  bottom-right (get cells [(inc y) (inc x)])
                  n-mas (cond-> 0
                          (x-mas? [top-left \A bottom-right]) inc
                          (x-mas? [top-right \A bottom-left]) inc)]
              (if (= n-mas 2)
                (inc total)
                total)))
          0))))

;; # Tests
(deftest test-2024-04
  (testing "part one"
    (is (= 2639 (part-1 input))))

  (testing "part two"
    (is (= 2005 (part-2 input)))))
