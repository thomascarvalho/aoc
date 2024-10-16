^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.11
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "11" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/parse-out-longs
       first))

(def input (->> (slurp (io/resource "inputs/2018/11.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "18"))

(defn hundred-digit [n]
  (mod (quot (or n 0) 100) 10))

(defn- fuel-power-level [[x y] serial-number]
  (let [rack-id (+ x 10)
        power-level (+ (* rack-id y) serial-number)
        p2 (* power-level rack-id)]
    (- (hundred-digit p2) 5)))

(defn get-cells [serial-number]
  (->> (for [x (range 1 301)
             y (range 1 301)
             :let [coords [x y]]]
         (hash-map coords (fuel-power-level coords serial-number)))
       (apply merge)))


;; ## Part 1
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-1
  [serial-number]
  (let [cells (get-cells serial-number)
        cells-with-total (for [x (range 1 (- 300 3))
                               y (range 1 (- 300 3))
                               :let [cells-on-square (mapv #(get cells %) [[x y] [(inc x) y] [(+ x 2) y]
                                                                           [x (inc y)] [(inc x) (inc y)] [(+ x 2) (inc y)]
                                                                           [x (+ y 2)] [(inc x) (+ y 2)] [(+ x 2) (+ y 2)]])]]
                           [[x y] (reduce + cells-on-square)])]
    (->> cells-with-total
         (reduce (fn [acc [coords power]]
                   (if (> power (second acc))
                     [coords power]
                     acc))
                 [[] -99])
         first
         (str/join ","))))

;; ## Part 2
(defn part-2
  [data]
  data)

(let [serial-number 18
      cells (get-cells serial-number)
      l-max 300
      cells-with-total (for [x (range 1 (inc l-max))
                             y (range 1 (inc l-max))
                             :let [n (inc (- l-max (max x y)))]]
                         [[x y] n (reduce + (for [i (range (inc n))
                                                  :let [tx (+ x i)
                                                        ty (+ y i)
                                                        coords [tx ty]]]
                                                 (get cells coords 0)))])]
  cells-with-total (->> cells-with-total
                          (reduce (fn [acc [coords w power]]
                                    (if (> power (nth acc 2))
                                      [coords w power]
                                      acc))
                                  [[] 0 -99])
                          #_(str/join ",")))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2018-11
  (testing "part one"
    (is (= "235,85" (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/test-render #'test-2018-11)
