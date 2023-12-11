^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.11
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.core.matrix :as ma]
            [test-util :as t]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "11" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-matrix))

(def input (->> (slurp (io/resource "inputs/2023/11.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."))

(defn expand
  ([m]
   (expand m 1))
  ([m pad]
   (let [empty-x (->> (ma/columns m)
                      (map-indexed
                       (fn [x col]
                         (when (every? #(= % \.) col)
                           x)))
                      (remove nil?)
                      set)

         empty-y (->> (ma/rows m)
                      (map-indexed
                       (fn [y row]
                         (when (every? #(= % \.) row)
                           y)))
                      (remove nil?)
                      set)]
     (for [[y r] (map-indexed (fn [y r] [y r]) m)
           [x c] (map-indexed (fn [x c] [x c]) r)
           :when (= c  \#)
           :let  [x-factor (count (filter #(> x %) empty-x))
                  y-factor (count (filter #(> y %) empty-y))
                  new-x (+ x (* pad x-factor))
                  new-y (+ y (* pad y-factor))]]
       [new-x new-y]))))

(defn get-pairs [galaxies]
  (->>
   (for [g1    galaxies
         g2    galaxies
         :when (not= g1 g2)]
     (sort [g1 g2]))
   set))

(defn get-pairs-distance-sum [pairs]
  (->> pairs
       (reduce (fn [total [g1 g2]]
                 (+ total (u/manhattan-distance g1 g2))) 0)))

;; ## Part 1
(defn part-1
  [initial-m]
  (->> (expand initial-m)
       get-pairs
       get-pairs-distance-sum))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn part-2
  [initial-m]
  (->> (dec 1000000)
       (expand initial-m)
       get-pairs
       get-pairs-distance-sum))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-11
  (testing "part one - example"
    (is (= 374 (part-1 input-example))))

  (testing "part one"
    (is (= 9556896 (part-1 input))))

  (testing "part two"
    (is (= 685038186836 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

