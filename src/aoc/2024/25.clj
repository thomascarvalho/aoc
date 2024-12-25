(ns aoc.2024.25
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.core.matrix :as ma]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (let [schemas (str/split data #"\n\n")]
    (reduce
     (fn [d schema]
       (let [m (u/to-matrix schema)
             first-row (ma/get-row m 0)
             cols (ma/columns m)
             lock? (every? #(= % \#) first-row)]
         (if lock?
           (update d 0 conj (mapv (fn [col] (- (count (filter #(= % \#) col)) 1)) cols))
           (update d 1 conj (mapv (fn [col] (- (count (filter #(= % \#) col)) 1)) cols)))))
     [[] []]

     schemas)))


;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/25.txt"))
                parser))

(def input-example (parser "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"))

;; Logic

;; ## Part 1
(defn part-1
  [[ln kn]]
  (->> (for [l ln 
             k kn
             :let [r (mapv + l k)]
             :when (every? #(<= % 5) r)]
         [l k])
       count))

;; ## Part 2
#_(defn part-2
    [data]
    data)

;; # Tests
#_{:nextjournal.clerk/visibility {:code   :show
                                  :result :hide}}
(deftest test-2024-25
  (testing "part one"
    (is (= 3317 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

