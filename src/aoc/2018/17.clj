^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.17
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [pathfinding :as pf]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "17" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn ->coords [[start-x end-x] [start-y end-y]]
  (for [x (range start-x (inc (or end-x start-x)))
        y (range start-y (inc (or end-y start-y)))]
    [x y]))


(defn parser [data]
  (->> data
       u/to-lines
       (mapcat (fn [l]
                 (let [[x y] (sort (str/split l #", "))]
                   (->coords (u/parse-out-longs x) (u/parse-out-longs y)))))
       (reduce (fn [m coords]
                 (assoc m coords \#)) {})))

(def input (->> (slurp (io/resource "inputs/2018/17.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504"))

(pf/draw-grid input-example)

(defn- get-max-y [cells]
  (->> cells
       (map (comp second first))
       (apply max)))


(defn- get-next-available-cell-on-column [cells [x y]]
  (let [[[tx ty]] (->> cells
                   (filter (fn [[[tx ty] v]]
                             (and (= tx x)
                                  (> ty y))))
                   (sort-by (comp second first))
                   first)]
    [tx (dec ty)]))

(defn- get-nexts-available-cells-on-row [cells [x y]]
  (let [[[left-x]] (->> cells
                      (filter (fn [[[tx ty] v]]
                                (and (< tx x)
                                     (= ty y))))
                      (sort-by ffirst >)
                      first)
        [[right-x]] (->> cells
                         (filter (fn [[[tx ty] v]]
                                   (and (> tx x)
                                        (= ty y))))
                         (sort-by ffirst)
                         first)]
    (mapv (fn [tx] [tx y]) (range (inc left-x) right-x))))


#_(let [cells input-example
        start-water [500 0]
        max-y (get-max-y cells)]

    (loop [last-inserted-cells []
           starting-points [start-water]
           cells cells
           step 0]
      (if (= step 5)
        (pf/draw-grid cells)
        (let [down-cell (get-next-available-cell-on-column cells (first starting-points))
              horizontal-cells (get-nexts-available-cells-on-row cells down-cell)
              inserted-cells horizontal-cells #_(concat down-cell horizontal-cells)]
          (recur inserted-cells
                 starting-points
                 (reduce (fn [cells coords]
                           (assoc cells coords \~)) cells inserted-cells)
                 (inc step)))))





    #_max-y)



;; ## Part 1
(defn part-1
  [data]
  data)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
#_(deftest test-2018-17
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

