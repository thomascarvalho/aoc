^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.11
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.core.matrix :as ma]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map #(str/split % #""))
       (map #(map parse-int %))
       ma/matrix))

(def input (->> (slurp (io/resource "inputs/2021/11.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"))

(defn in-bounds? [x y max-x max-y]
  (and (>= x 0) (< x max-x) (>= y 0) (< y max-y)))

(defn matrix-inc
  [matrix]
  (ma/add matrix 1))

(defn matrix->hash-map
  [matrix]
  (->>
   matrix
   (ma/emap-indexed (fn [i c]  (hash-map (vec i) c)))
   (mapcat concat)
   (apply merge)))

(defn set-unique [matrix [x y] value]
  (ma/mset matrix x y value))

(defn set-multiples [matrix coords-to-upd value]
  (loop [m      matrix
         coords coords-to-upd]
    (if-let [c (first coords)]
      (recur (set-unique m c value) (next coords))
      m)))

(defn matrix-filter [matrix filter-fn]
  (let [matrix-map    (matrix->hash-map matrix)]
    (filter (fn [[coords v]] (filter-fn coords v)) matrix-map)))

(defn get-neighbours [matrix [x y]]
  (let [max-x             (count matrix)
        max-y             (count (first matrix))
        directions        [[-1 -1] [-1 0] [-1 1]  [0 -1]  [0 1]  [1 -1] [1 0] [1 1]]
        neighbours-coords (filter #(in-bounds? (first %) (second %) max-x max-y)
                                  (map #(vector (+ x (first %)) (+ y (second %))) directions))]
    (matrix-filter matrix (fn [c _v]
                            (some #(= c %) neighbours-coords)))))

(defn set-flashes [matrix]
  (let [cells (matrix-filter matrix (fn [_coords v] (and (not= v :F) (> v 9))))]
    (set-multiples matrix (map first cells) :F)))

(defn reset-flashes [total-flashes-a matrix]
  (let [cells (matrix-filter matrix (fn [_coords v] (= v :F)))]    
    [(set-multiples matrix (map first cells) 0) (+ total-flashes-a (count cells)) (count cells)]))
    

(defn process-flashes [matrix]
  (let [flashed-cells        (matrix-filter matrix (fn [_coords v] (= v :F)))
        flashed-cells-coords (map first flashed-cells)]
    (if (seq flashed-cells-coords)
      (loop [m                       matrix
             flashed-coords          flashed-cells-coords
             previous-flashed-coords []]
        (if (seq flashed-coords)
          (let [c                       (first flashed-coords)
                neighbours              (get-neighbours m c)
                matrix-with-new-flashes (->>
                                         (reduce (fn [s [[x y] v]]
                                                   (if-not (= v :F)
                                                     (update-in s [x y] inc)
                                                     s))
                                                 m neighbours)
                                         set-flashes)
                new-flashes-coords      (matrix-filter matrix-with-new-flashes
                                                       (fn [coords v] (and (= v :F)
                                                                           (not (some #(= coords %) (concat flashed-coords previous-flashed-coords))))))]

            (recur (or matrix-with-new-flashes m)
                   (next (concat flashed-coords (map first new-flashes-coords)))
                   (conj previous-flashed-coords c)))
          m))
      matrix)))

(defn next-step [total-flashes matrix]
  (->> matrix
       matrix-inc
       set-flashes
       process-flashes
       (reset-flashes total-flashes)))

;; ## Part 1
(defn part-1
  [matrix]
  (loop [m             matrix
         step          0
         total-flashes 0]
    (if (= step 100)
      total-flashes
      (let [[m total _current-flashes] (next-step total-flashes m)]
        (recur m (inc step) total)))))
  ;
  


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [matrix]
  (loop [m             matrix
         step          0
         total-flashes 0]
    (let [[m total current-flashes] (next-step total-flashes m)]
      (if (= current-flashes 100)
        (inc step)
        (recur m (inc step) total)))))

  ;
  



;; Tests
(deftest test-2021-11
  (testing "part one"
    (is (= 1732 (part-1 input))))

  (testing "part two"
    (is (= 290 (part-2 input)))))
