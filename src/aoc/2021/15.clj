^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.15
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.core.matrix :as matrix]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "15" "2021"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(def dirs #{[-1 0] [1 0] [0 1] [0 -1]})

(defn parser-part-1 [data]
  (->> data
       str/split-lines
       (map-indexed
        (fn [y row]
          (map-indexed
           (fn [x risk]
             {[y x] (parse-int risk)}) (str/split row #""))))
       (mapcat concat)
       (apply merge)))

(def input (slurp (io/resource "inputs/2021/15.txt")))
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")


(defn get-risk [cells coords]
  (get cells coords))

(defn neighbours [[y x] cells]
  (for [[y2 x2] dirs
        :let    [target-coords [(+ y y2) (+ x x2)]
                 risk (get-risk cells target-coords)]
        :when   risk]
    [target-coords risk]))

(defn get-neighbours-edges [[coords] cells]
  (->> (neighbours coords cells)
       (map (fn [[target-coords risk]]
              [coords target-coords {:risk risk}]))))

(defn get-edges [cells]
  (->>
   (mapcat #(get-neighbours-edges % cells) cells)))

(defn find-shortest-path-cost [cells]
  (let [edges         (get-edges cells)
        g             (-> (uber/graph)
                          (uber/add-edges* edges))
        all-coords    (keys cells)
        [end-y max-x] (reduce (fn [[prev-y prev-x] [y x]]
                                [(max prev-y y) (max prev-x x)]) all-coords)]


    (->
     (alg/shortest-path g {:start-node [0 0]
                           :end-node   [end-y max-x]
                           :cost-fn    (fn [[a b]]
                                         (get-risk cells b))})
     :cost)))

;; ## Part 1
(defn part-1
  [input]
  (-> input
      parser-part-1
      find-shortest-path-cost))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
#_(matrix-sel/sel [[-2 -1] [0 1]] matrix-sel/end matrix-sel/end)

(defn parser-part-2 [input]
  (->> input
       str/split-lines
       (map #(str/split % #""))
       (map #(map parse-int %))
       matrix/matrix))

(defn increment-matrix [m]

  (let [m-incremented   (matrix/add m 1)
        cells-to-update (->>  m-incremented
                              (matrix/emap-indexed (fn [coords value]
                                                     (when (= value 10) (vec coords))))
                              (mapcat concat)
                              (remove nil?))]
    (loop [cells cells-to-update
           m     m-incremented]
      (if (seq cells)
        (let [[x y] (first cells)]
          (recur (next cells) (matrix/mset m x y 1)))
        m))))

(defn part-2
  [input]
  (let [m1    (parser-part-2 input)
        m2    (increment-matrix m1)
        m3    (increment-matrix m2)
        m4    (increment-matrix m3)
        m5    (increment-matrix m4)
        m6    (increment-matrix m5)
        m7    (increment-matrix m6)
        m8    (increment-matrix m7)
        m9    (increment-matrix m8)

        m     (matrix/join
               (matrix/join-along 1 m1 m2 m3 m4 m5)
               (matrix/join-along 1 m2 m3 m4 m5 m6)
               (matrix/join-along 1 m3 m4 m5 m6 m7)
               (matrix/join-along 1 m4 m5 m6 m7 m8)
               (matrix/join-along 1 m5 m6 m7 m8 m9))

        cells (->> m
                   (matrix/emap-indexed
                    (fn [coords risk]
                      {(vec coords) risk}))
                   (mapcat concat)
                   (apply merge))]

    (->
     cells
     find-shortest-path-cost))

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2021-15
    (testing "part one"
      (is (= 755 (part-1 input))))

    (testing "part two"
      (is (= 3016 (part-2 input)))))
