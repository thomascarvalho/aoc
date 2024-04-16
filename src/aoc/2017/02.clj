^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.02
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.math.combinatorics :as combo]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "02" "2017"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map u/parse-out-longs)))

(def input (->> (slurp (io/resource "inputs/2017/02.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "5 1 9 5
7 5 3
2 4 6 8"))

(let [data input-example]
  (reduce (fn [t nums]
            (+ t (- (apply max nums) (apply min nums)))) 0 data))

;; ## Part 1
(defn part-1
  [data]
  (reduce (fn [t nums]
            (+ t (- (apply max nums) (apply min nums)))) 0 data))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(def input-example-2 (parser "5 9 2 8
9 4 7 3
3 8 6 5"))

(defn part-2
  [input]
  (->>
   (for [l input]
     (->> (combo/permuted-combinations l 2)
          (reduce (fn [_ [a b]]
                    (let [d (/ a b)]
                      (when (int? d) (reduced d)))))))
   (reduce +)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-02
  (testing "part one"
    (is (= 45351 (part-1 input))))

  (testing "part two"
    (is (= 275 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(t/test-render #'test-2017-02)