^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.23
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [pathfinding :as pf]
            [clojure.test :refer :all]))

^{::clerk/visibility {:code   :hide
                      :result :hide}}
#_(ec/install!)

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "23" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-matrix
       pf/decode-matrix))

(def input (->> (slurp (io/resource "inputs/2023/23.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#"))

(defn neighbors [cell grid part]
  (let [[y x] cell]
    (for [pos   [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]]
          :let  [[ty tx] pos
                 v (grid pos)]
          :when (and v (not= v \#))]
      (if (= part :p2)
        pos
        (case v
          \^ [(dec ty) tx]
          \v [(inc ty) tx]
          \< [ty (dec tx)]
          \> [ty (inc tx)]
          \. pos)))))

(defn all-paths [grid start end part]
  (loop [stack     [[start #{start} [start]]]
         all-paths []]
    (if (empty? stack)
      all-paths
      (let [[[ current visited path] & remaining-stack] stack
            next-neighbors                             (filter #(not (contains? visited %)) (neighbors current grid part))]
        (recur (reduce (fn [stk neighbor]
                         (conj stk [neighbor (conj visited neighbor) (conj path neighbor)]))
                       remaining-stack
                       next-neighbors)
               (if (= current end)
                 (conj all-paths path)
                 all-paths))))))
;; ## Part 1
(defn part-1
  [{:keys [width height cells]}]
  (let [start [0 1]
        end   [(dec height) (- width 2)]]
    (->>
     (all-paths cells start end :p1)
     (map (fn [path]
            (reduce (fn [t [p c]] (+ t (u/manhattan-distance p c))) 0 (partition 2 1 path))))
     sort
     last)))

  ;


;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input-example)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [{:keys [width height cells]}]
  (let [start [0 1]
        end   [(dec height) (- width 2)]]
    (->>
     (all-paths cells start end :p2)
     (map (comp dec count))
     sort
     last))
  
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-23
  (testing "part one - example"
      (is (= 94 (part-1 input-example))))
  
  (testing "part one"
    (is (= 2086 (part-1 input))))

  (testing "part two - example"
    (is (= 154 (part-2 input-example))))
  
  #_(testing "part two"
      (is (= 1 (part-2 input)))))

#_{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
