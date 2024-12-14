^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.14
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.core.matrix :as ma]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution

(defn parser [data]
  (->> data
       u/to-matrix))

(def input (->> (slurp (io/resource "inputs/2023/14.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."))

(defn slide-column-north [m index-col]
  (loop [i   0
         col (ma/get-column m index-col)]
    (if (>= i (count col))
      col
      (let [c (nth col i)]
        (if (= c \O)
          (cond
            (zero? i) (recur (inc i) col)
            (= (nth col (dec i)) \.) (recur (dec i) (->
                                                     col
                                                     (ma/mset (dec i) \O)
                                                     (ma/mset i \.)))
            :else (recur (inc i) col))
          (recur (inc i) col))))))

(defn slide-column-south [m index-col]
  (let [original-col (ma/get-column m index-col)
        max-y        (dec (count original-col))]
    (loop [i   max-y
           col original-col]
      (if (< i 0)
        col
        (let [c (nth col i)]
          (if (= c \O)
            (cond
              (= i max-y) (recur (dec i) col)
              (= (nth col (inc i)) \.) (recur (inc i) (->
                                                       col
                                                       (ma/mset (inc i) \O)
                                                       (ma/mset i \.)))
              :else (recur (dec i) col))
            (recur (dec i) col)))))))

(defn slide-row-west [m index-row]
  (loop [i   0
         row (ma/get-row m index-row)]
    (if (>= i (count row))
      row
      (let [c (nth row i)]
        (if (= c \O)
          (cond
            (zero? i) (recur (inc i) row)
            (= (nth row (dec i)) \.) (recur (dec i) (->
                                                     row
                                                     (ma/mset (dec i) \O)
                                                     (ma/mset i \.)))
            :else (recur (inc i) row))
          (recur (inc i) row))))))

(defn slide-row-east [m index-row]
  (let [original-row (ma/get-row m index-row)
        max-x        (dec (count original-row))]
    (loop [i   max-x
           row original-row]
      (if (< i 0)
        row
        (let [c (nth row i)]
          (if (= c \O)
            (cond
              (= i max-x) (recur (dec i) row)
              (= (nth row (inc i)) \.) (recur (inc i) (->
                                                       row
                                                       (ma/mset (inc i) \O)
                                                       (ma/mset i \.)))
              :else (recur (dec i) row))
            (recur (dec i) row)))))))

(def spin-cycle [:N :W :S :E])

(defn slide-columns [m dir]
  (let [slide (case dir
                :N slide-column-north
                :S slide-column-south)]
    (->>
     (for [x (range 0 (ma/column-count m))]
       (slide m x))
     (ma/transpose))))

(defn slide-rows [m dir]
  (let [slide (case dir
                :W slide-row-west
                :E slide-row-east)]
    (->>
     (for [x (range 0 (count m))]
       (slide m x)))))

(defn spin-matrix [original-matrix]
  (reduce (fn [m dir]
            (case dir
              (:N :S) (slide-columns m dir)
              (:W :E) (slide-rows m dir))) original-matrix spin-cycle))

;; ## Part 1
(defn part-1
  [m]
  (let [height   (count m)
        tilted-m (slide-columns m :N)]

    (->>
     (for [[y row] (map-indexed (fn [y row] [y row]) tilted-m)
           c       row
           :when   (= c \O)]
       (- height y))
     (reduce +))))

  ;
  

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn north-support-beams [m]
  (let [height   (count m)]
    (->>
     (for [[y row] (map-indexed (fn [y row] [y row]) m)
           c       row
           :when   (= c \O)]
       (- height y))
     (reduce +))))

#_(defn find-repeating-patterns [min-pattern-length vector]
    (let [n (count vector)]
      (loop [start    0
             patterns {}]
        (if (>= start n)
          patterns
          (let [patterns (loop [end      (inc start)
                                patterns patterns]
                           (if (>= end n)
                             patterns
                             (let [pattern (subvec vector start end)]
                               (if (and (>= (count pattern) min-pattern-length)
                                        (some #(= pattern (subvec % 0 (count pattern)))
                                              (map vec (partition (count pattern) 1 (drop end vector)))))
                                 (recur (inc end) (update patterns pattern conj start))
                                 (recur (inc end) patterns)))))]
            (recur (inc start) patterns))))))

(defn find-first-repeating-pattern [min-pattern-length vector]
  (let [n (count vector)]
    (loop [start 0]
      (when (< start (- n min-pattern-length))
        (let [pattern (subvec vector start (+ start min-pattern-length))]
          (if (some #(= pattern (subvec % 0 (count pattern)))
                    (map vec (partition (count pattern) 1 (drop (+ start min-pattern-length) vector))))
            {:pattern pattern
             :index   start}
            (recur (inc start))))))))

#_(defn find-first-repeating-pattern [min-pattern-length vector]
   (let [n (count vector)]
     (loop [start        0
            first-repeat nil]
       (if (>= start n)
         first-repeat
         (let [result (loop [end          (inc start)
                             first-repeat first-repeat]
                        (if (>= end n)
                          first-repeat
                          (let [pattern (subvec vector start end)]
                            (if (and (>= (count pattern) min-pattern-length)
                                     (some #(= pattern (subvec % 0 (count pattern)))
                                           (map vec (partition (count pattern) 1 (drop end vector)))))
                              {:pattern pattern
                               :index   start}
                              (recur (inc end) first-repeat)))))]
           (recur (inc start) result))))))



(defn part-2
  [m]
  (let [{:keys [pattern index cycles]
         :as   a} (->>
                   (reduce (fn [[o beams cycles] _n]
                             (let [p (find-first-repeating-pattern 10 beams)]
                               (if p
                                 (reduced (-> p
                                              (assoc :cycles (inc cycles))
                                              #_(update :index + (count (drop (:index p) (:pattern p))))))
                                 (let [m (spin-matrix o)]
                                   [m (conj beams (north-support-beams m)) (inc cycles)])))) [m [] 0] (range 500)))]


    (u/nth-cycling pattern (- 1000000000 cycles))))



  ;
  

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-14
  (testing "part one"
      (is (= 110407 (part-1 input))))

  (testing "part two - example"
    (is (= 64 (part-2 input-example))))

  (testing "part two"
    (is (= 87273 (part-2 input)))))
