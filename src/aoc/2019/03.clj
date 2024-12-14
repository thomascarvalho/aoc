^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2019.03
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.set :as cset]
            [clojure.test :refer :all]))

;; # Solution

(defn parser [data]
  (->> data
       str/split-lines
       (map (fn [l]
              (->> l
                   (re-seq #"(\w)(\d+)")
                   (map (fn [[_ dir n]]
                          [(keyword dir) (parse-int n)])))))))

;; First things first, let's load our input and parse it
(def input (->> (slurp (io/resource "inputs/2019/03.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example #_(parser "R8,U5,L5,D3
                            U7,R6,D4,L4")

  (parser "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83"))


(defn get-wire-coords [[x y] [move dist']]
  (let [dist (inc dist')]
    (case move
      :R (map #(vector % y) (range x (+ x dist)))
      :L (map #(vector % y) (range x (- x dist) -1))
      :U (map #(vector x %) (range y (+ y dist)))
      :D (map #(vector x %) (range y (- y dist) -1)))))


;; ## Part 1
(defn part-1
  [input]
  (->>
   (for [wires input]
     (loop [moves           wires
            pos             [0 0]
            all-wire-coords []]
       (if-let [move (first moves)]
         (let [wire-coords (get-wire-coords pos move)]
           (recur (next moves) (last wire-coords) (concat all-wire-coords wire-coords)))
         (set all-wire-coords))))
   (apply cset/intersection)
   (filter #(not= % [0 0]))
   (map #(u/manhattan-distance [0 0] %))
   (reduce min)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]
  (let [[wire-1 wire-2] (for [wires input]
                          (loop [moves           wires
                                 pos             [0 0]
                                 all-wire-coords []]
                            (if-let [move (first moves)]
                              (let [wire-coords (get-wire-coords pos move)]
                                (recur (next moves) (last wire-coords) (concat all-wire-coords (next wire-coords))))
                              all-wire-coords)))
        intersections   (-> (cset/intersection (set wire-1) (set wire-2))
                            (disj [0 0]))]
    (->>
     (for [inter intersections]
       (+ (->> wire-1
               (take-while #(not= % inter))
               count
               inc)
          (->> wire-2
               (take-while #(not= % inter))
               count
               inc)))
     (reduce min))))

;; Tests
(deftest test-2019-03
  (testing "part one"
    (is (= 709 (part-1 input))))

  (testing "part two"
    (is (= 13836 (part-2 input)))))
