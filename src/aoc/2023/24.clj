^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.24
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.math.numeric-tower :as nt]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "24" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map u/parse-out-longs)))

(def input (->> (slurp (io/resource "inputs/2023/24.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3"))

(defn trace [[[x y z] [vx vy vz]] [min max]]
  (->>
   (iterate (fn [[x y z]]
              [(+ x vx) (+ y vy) 0 #_(+ z vz)]) [x y 0])
   (take-while (fn [[x y z]]
                 (and (<= min x max)
                      (<= min y max))))
   (filter (fn [[x y z]]
             (and
              (<= min x max)
              (<= min y max))))))

(defn vector-sub [v1 v2]
  (mapv - v1 v2))

(defn vector-cross [v1 v2]
  [(- (* (v1 1) (v2 2)) (* (v1 2) (v2 1)))
   (- (* (v1 2) (v2 0)) (* (v1 0) (v2 2)))
   (- (* (v1 0) (v2 1)) (* (v1 1) (v2 0)))])

(defn scalar-mult [s v]
  (mapv #(* s %) v))

(defn point-on-line? [point line]
  (let [[p1 p2]    line
        line-vec   (vector-sub p2 p1)
        point-vec  (vector-sub point p1)
        cross-prod (vector-cross line-vec point-vec)]
    (and (zero? (cross-prod 0))
         (zero? (cross-prod 1))
         (zero? (cross-prod 2)))))

(defn dot [v1 v2]
  (reduce + (map * v1 v2)))

(defn line-intersection-3d [line1 line2]
  (let [[p1 p2] line1
        [p3 p4] line2
        p13     (vector-sub p1 p3)
        p43     (vector-sub p4 p3)
        p21     (vector-sub p2 p1)
        d1343   (dot p13 p43)
        d4321   (dot p43 p21)
        d1321   (dot p13 p21)
        d4343   (dot p43 p43)
        d2121   (dot p21 p21)
        denom   (- (* d4343 d2121) (* d4321 d4321))
        numer   (- (* d1343 d4321) (* d1321 d4343))]
    (if (zero? denom)
      nil
      (let [mu                 #_(float) (/ numer denom)
            intersection-point (mapv + p1 (scalar-mult mu p21))]
        #_intersection-point
        (if (and (point-on-line? intersection-point line1)
                   (point-on-line? intersection-point line2))
            (mapv float intersection-point)
            nil)))))

;; ## Part 1
(defn part-1
  [hailstones]

  (let [bounds [7 27]
        paths  (for [h     hailstones
                     :let  [p (trace (partition 3 h) bounds)]
                     :when (seq p)]
                 [h p])]
    (for [[h1 p1]    paths
          [h2 p2]    paths
          :let  [intersect (line-intersection-3d [(first p1) (last p1)] [(first p2) (last p2)])]
          :when (and (not= p1 p2) intersect)]
      [h1 h2 intersect]))
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


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-24
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

(part-1 input-example)