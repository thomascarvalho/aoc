^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.18
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [pathfinding :as pf]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "18" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       pf/decode-matrix))

(def input (->> (slurp (io/resource "inputs/2018/18.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;; 
;;  Example
(def input-example (parser ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|."))

(defn neighbors [grid cell]
  (let [[y x] cell]
    (filter #(grid %)
            [[(dec y) x]
             [(inc y) x]
             [y (dec x)]
             [y (inc x)]
             [(dec y) (dec x)]
             [(inc y) (inc x)]
             [(dec y) (inc x)]
             [(inc y) (dec x)]])))


(defn neighbors-vals [grid cell type]
   (let [[y x] cell]
    (filter (fn [coords] (= type (grid coords)))
            [[(dec y) x]
             [(inc y) x]
             [y (dec x)]
             [y (inc x)]
             [(dec y) (dec x)]
             [(inc y) (inc x)]
             [(dec y) (inc x)]
             [(inc y) (dec x)]])))

;; ## Part 1
(defn part-1
  ([data]
   (part-1 data 10))
  ([data minutes] 
   (let [{:keys [cells]} data
         freqs (->> (loop [cells cells
                           minute 0]
                      (if (= minute minutes)
                        cells
                        (recur
                         (reduce (fn [m [coords v]]
                                   (case v
                                     \. (if (>= (count (neighbors-vals cells coords \|) ) 3)
                                          (assoc m coords \|)
                                          m)
                                     \|  (if (>= (count (neighbors-vals cells coords \#)) 3)
                                           (assoc m coords \#)
                                           m)
                                     \# (let [lumberyards (neighbors-vals cells coords \#) 
                                              trees (neighbors-vals cells coords \|)]
                                          (if (and (>= (count trees) 1) (>= (count lumberyards) 1))
                                            m
                                            (assoc m coords \.)))))
                                 cells cells)
                         (inc minute))))
                    (map val)
                    frequencies)]
     (* (get freqs \#) (get freqs \|)))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2018-18
  (testing "part one"
    (is (= 360720 (time (part-1 input)))))

  #_(testing "part two"
      (is (= 1 (part-1 input 1000000000)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/test-render #'test-2018-18)
