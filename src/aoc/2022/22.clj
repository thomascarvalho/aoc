^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.22
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.core.matrix :as mat]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "22" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it


(defn create-matrix [lines]
  (->> lines
       (drop-last 2)
       (map #(->>
              (str/split % #"")

              (map (fn [c]
                     (when-not (= c " ") c)))))
       mat/matrix))

(defn create-actions [lines]
  (->>  lines
        (take-last 1)
        (first)
        (re-seq #"(\d+)([RL])?")
        (map #(drop 1 %))
        (map (fn [[move direction]]
               [(parse-int move) (keyword direction)]))
        flatten
        (remove nil?)))

(defn parser [data]
  (let [lines      (str/split-lines data)
        matrix     (create-matrix lines)
        moves-line (create-actions lines)]
    (vec [matrix moves-line])))

(def input (->> (slurp (io/resource "inputs/2022/22.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"))


(def cell-types {:empty "."
                 :wall  "#"})

(defn is-empty? [cell-type]
  (= cell-type (:empty cell-types)))

(defn is-wall? [cell-type]
  (= cell-type (:wall cell-types)))

(defn is-none? [cell-type]
  (nil? cell-type))

(defn get-cell [matrix [x y]]
  [[x y] (mat/mget matrix y x)])

(defn get-row [matrix row-idx]
  (map-indexed (fn [x v]
                 [[x row-idx] v]) (mat/get-row matrix row-idx)))

(defn get-column [matrix col-idx]
  (map-indexed (fn [y v]
                 [[col-idx y] v])
               (mat/get-column matrix col-idx)))

(defn filter-row-or-col [row filter-fn]
  (filter (fn [[coords v]] (filter-fn [coords v])) row))

(defn no-nil-filter [cells]
  (filter-row-or-col cells (fn [[_ v]] (not (nil? v)))))

(defn get-row-without-nil [matrix row-idx]
  (->> (get-row matrix row-idx)
       no-nil-filter))

(defn get-column-without-nil [matrix col-idx]
  (->> (get-column matrix col-idx)
       no-nil-filter))

(defn first-empty-in-row [matrix row-idx]
  (-> matrix
      (get-row row-idx)
      (filter-row-or-col (fn [[_ v]] (is-empty? v)))
      first))

(def directions (cycle [:EAST :SOUTH :WEST :NORTH]))

(defn get-directions [directions turn]
  (case turn
    :R (next (take 5 (cycle directions)))
    :L (concat (drop 3 directions) (take 3 directions))))

(defn move-to [matrix current-pos current-direction ^Number step]
  (let [[x y]         current-pos
        get-movements (fn [row-or-column]
                        (let [current-idx     (.indexOf row-or-column [current-pos "."])
                              reordered-cells (->
                                               (drop current-idx row-or-column)
                                               (concat (take current-idx row-or-column)))]
                          (->>
                           (cycle reordered-cells)
                           (take (inc step))
                           (take-while (fn [[_coords v]] (not (is-wall? v)))))))]
    (->
     (case current-direction
       :EAST   (get-movements (get-row-without-nil matrix y))
       :SOUTH (get-movements (get-column-without-nil matrix x))
       :WEST  (get-movements (reverse (get-row-without-nil matrix y)))
       :NORTH (get-movements (reverse (get-column-without-nil matrix x))))
     last
     first
     (or current-pos))))


;; ## Part 1
(defn part-1
  [[matrix initial-actions]]
  (loop [actions    initial-actions
         ;; m          initial-matrix
         pos        (-> (first-empty-in-row matrix 0)
                        first)
         directions (cycle [:EAST :SOUTH :WEST :NORTH])]

    (if (seq actions)
      (let [action    (first actions)
            direction (first directions)]
        (if (number? action)
          (recur (next actions) (move-to matrix pos direction action) directions)
          (recur (next actions) pos (get-directions directions action))))

      (let [[final-x final-y] (map inc pos)
            direction-ratio   (case (first directions)
                                :NORTH 3
                                :EAST 0
                                :SOUTH 1
                                :WEST 2)]
        (+ (* 1000 final-y)  (* 4 final-x) direction-ratio))))


  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


(second input-example)

;; Tests
(deftest test-2022-22
  (let [m (first input-example)]
    (testing "get first non empty"
      (is (= [[8 0] "."] (first-empty-in-row  m 0)))
      (is (= [[0 4] "."] (first-empty-in-row m 4))))

    (testing "get row"
      (is (= [[[0 0] nil]
              [[1 0] nil]
              [[2 0] nil]
              [[3 0] nil]
              [[4 0] nil]
              [[5 0] nil]
              [[6 0] nil]
              [[7 0] nil]
              [[8 0] "."]
              [[9 0] "."]
              [[10 0] "."]
              [[11 0] "#"]] (get-row m 0)))

      (is (= [[[8 0] "."]
              [[9 0] "."]
              [[10 0] "."]
              [[11 0] "#"]] (get-row-without-nil m 0))))

    (testing "get cells"
      (is (= [[9 0] "."] (get-cell m [9 0])))
      (is (= [[3 4] "#"] (get-cell m [3 4]))))

    (testing "get column"
      (is (= [[[8 0] "."]
              [[8 1] "."]
              [[8 2] "#"]
              [[8 3] "."]
              [[8 4] "."]
              [[8 5] "#"]
              [[8 6] "."]
              [[8 7] "."]
              [[8 8] "."]
              [[8 9] "."]
              [[8 10] "."]
              [[8 11] "."]] (get-column m 8)))

      (is (= [[[5 4] "."]
              [[5 5] "."]
              [[5 6] "."]
              [[5 7] "."]] (get-column-without-nil m 5))))

    (testing "turns directions"
      (is (= [:SOUTH :WEST :NORTH :EAST] (get-directions [:EAST :SOUTH :WEST :NORTH] :R)))
      (is (= [:NORTH :EAST :SOUTH :WEST] (get-directions [:EAST :SOUTH :WEST :NORTH] :L)))
      (is (= [:WEST :NORTH :EAST :SOUTH] (get-directions [:SOUTH :WEST :NORTH :EAST] :R)))
      (is (= [:EAST :SOUTH :WEST :NORTH] (get-directions [:SOUTH :WEST :NORTH :EAST] :L))))


    (testing "moves"

      (is (= [10 0] (move-to m [8 0] :EAST 10)))
      (is (= [10 0] (move-to m [9 0] :EAST 1)))
      (is (= [8 0] (move-to m [8 0] :WEST 10)))
      (is (= [8 1] (move-to m [10 1] :EAST 4)))
      (is (= [10 4] (move-to m [10 1] :SOUTH 3)))
      (is (= [10 10] (move-to m [10 1] :NORTH 3)))

      ;; 
      )

    ;;
    )

  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))


#_(part-1 input)