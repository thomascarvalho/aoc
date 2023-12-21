^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.21
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.math.numeric-tower :as nt]
            [test-util :as t]
            [pathfinding :as pf]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "21" "2023"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-matrix
       pf/decode-matrix))

(def input (->> (slurp (io/resource "inputs/2023/21.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."))

(defn starting-cell [cells]
  (->> cells
       (filter #(-> % val (= \S)))
       first))

(def dirs [[0 1] [0 -1] [1 0] [-1 0]])

(defn neighbours [cells [y x]]
  (for [[delta-y delta-x] dirs
        :let              [t-pos [(+ y delta-y) (+ x delta-x)]
                           v (get cells t-pos)]
        :when             (= v \.)]
    t-pos))


;; ## Part 1
(defn part-1
  [{:keys [width height cells]}]
  (let [[starting-coords _] (starting-cell cells)
        total-steps         64]
    (->>
     (loop [distance  0
            positions #{starting-coords}]
       (if (= distance total-steps)
         positions
         (->>
          (for [p positions
                n (neighbours cells p)]
            n)
          set
          (recur (inc distance)))))
     count
     inc)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-1 input)

(defn neighbours-p2 [cells [y x] width height starting-coords]
  (for [[delta-y delta-x] dirs
        :let              [final-y (+ y delta-y)
                           final-x (+ x delta-x)
                           final-coords [final-y final-x]
                           t-pos [(mod final-y height) (mod final-x width)]
                           v (get cells t-pos)]
        :when             (and
                           (not= starting-coords final-coords)
                           (or (= v \.)
                               (= v \S)))]
    final-coords))

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [{:keys [width height cells]}]
  (reduce nt/lcm [50 1594 6536])

  (/ 130229800 (/ 1000 50))
  #_(let [[starting-coords _] (starting-cell cells)
        total-steps         100]
    (->>
     (loop [distance  0
            positions #{starting-coords}]
       (if (= distance total-steps)
         positions
         (->>
          (for [p positions
                n (neighbours-p2 cells p width height starting-coords)]
            n)
          set
          (recur (inc distance)))))
     count
     inc))

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-21
  #_(testing "part one"
    (is (= 3615 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

(part-2 input-example)