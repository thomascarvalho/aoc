^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.05
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "05" "2017"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/parse-out-longs
       (into [])))

(def input (->> (slurp (io/resource "inputs/2017/05.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "0
3
0
1
-3"))

;; ## Part 1
(defn part-1
  [data]
  (let [nums data]
    (loop [idx  0
           step 0
           nums nums]
      (let [v (get nums idx)]
        (if-not v
          step
          (recur
           (+ idx v)
           (inc step)
           (update nums idx inc)))))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [nums data]
    (loop [idx  0
           step 0
           nums nums]
      (let [v (get nums idx)]
        (if-not v
          step
          (recur
           (+ idx v)
           (inc step)
           (update nums idx (if (>= v 3) dec inc))))))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-05
  (testing "part one"
    (is (= 373543 (part-1 input))))

  (testing "part two"
    (is (= 27502966 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(t/test-render #'test-2017-05)