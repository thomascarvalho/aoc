^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2016.03
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "03" "2016"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map #(u/parse-out-longs %))))

(def input (->> (slurp (io/resource "inputs/2016/03.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))

;; ## Part 1
(defn part-1
  [data]
  (->> data
       (reduce
        (fn [r row]
          (if (every? true? (map (fn [[a b c]]
                                   (> (+ a b) c)) (combo/permutations row)))
            (inc r)
            r)) 0)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (->>
   (for [x [0 1 2]]
     (->> (map #(nth % x) data)
          (partition 3)
          (reduce
           (fn [r row]
             (if (every? true? (map (fn [[a b c]]
                                      (> (+ a b) c)) (combo/permutations row)))
               (inc r)
               r)) 0)))
   (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2016-03
  (testing "part one"
    (is (= 862 (part-1 input))))

  (testing "part two"
    (is (= 1577 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results