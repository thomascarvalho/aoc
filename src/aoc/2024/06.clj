(ns aoc.2024.06
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [pathfinding :as pf]
            [medley.core :as m]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Solution
(defn parser [data]
  (->> data
       u/to-matrix
       pf/decode-matrix))

(def input (->> (slurp (io/resource "inputs/2024/06.txt"))
                parser))

;;  Example
(def input-example (parser "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."))

(def DIRECTIONS [[-1 0] [0 1] [1 0] [0 -1]])

(defn next-coords [position direction-idx]
  (mapv + position (get DIRECTIONS (mod direction-idx 4))))

(defn move [position direction-idx cells]
  (let [next-pos (next-coords position direction-idx)]
    (when-let [v (get cells next-pos)]
      (if (= v \#)
        [position (mod (inc direction-idx) 4)]
        [next-pos direction-idx]))))

(defn get-start [cells]
  (ffirst (m/filter-vals (fn [v] (= v \^)) cells)))

;; ## Part 1
(defn part-1
  [data]
  (let [{:keys [cells]} data
        start-position (get-start cells)
        cells (assoc cells start-position \.)]
    (loop [position start-position
           direction-idx 0
           visited #{start-position}]
      (if-let [[new-position new-direction-idx] (move position direction-idx cells)]
        (recur
         new-position
         new-direction-idx
         (conj visited new-position))
        (count visited)))))


;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}


(defn is-infinite-loop? [start-position cells]
  (loop [position start-position
         direction-idx 0
         path #{[start-position 0]}
         step 0]
    (when-let [p (move position direction-idx cells)]
      (if (path p)
        true
        (let [[new-pos direction-new-idx] p]
          (recur
           new-pos
           direction-new-idx
           (conj path p)
           (inc step)))))))

(defn part-2
  [data]
  (let [{:keys [cells]} data
        start-position (get-start cells)]
    (->> (m/filter-vals (fn [v] (= v \.)) cells)
         (reduce (fn [total [coords]]
                   (if (is-infinite-loop? start-position (-> cells
                                                             (assoc start-position \.)
                                                             (assoc coords \#)))
                     (inc total)
                     total))
                 0))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-06
  (testing "part one"
    (is (= 4964 (part-1 input))))

  #_(testing "part two"
      (is (= 1740 (part-2 input)))))

