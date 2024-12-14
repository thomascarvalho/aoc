^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.09
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.test :refer :all]
            [clojure.math.combinatorics :as combo]))


;; # Solution

(defn parser [data]
  (->> data
       u/to-lines
       (map #(let [[_ from to dist] (re-find #"(\w+) to (\w+) = (\d+)" %)]
               (hash-map (sort [from to]) (parse-long dist))))
       (apply merge)))

(def input (->> (slurp (io/resource "inputs/2015/09.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141"))

;; ## Part 1
(defn part-1
  [edges]
  (let [locations (->> edges
                       keys
                       (apply (comp set concat)))]
    (->>
     (combo/permutations locations)
     #_(take 1)
     (reduce (fn [t locs]
               (let [dist (reduce (fn [d edge]
                                    (+ d (get edges (sort edge)))) 0 (partition 2 1 locs))]
                 (min t dist))) Double/MAX_VALUE))))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [edges]
  (let [locations (->> edges
                       keys
                       (apply (comp set concat)))]
    (->>
     (combo/permutations locations)
     (reduce (fn [t locs]
               (let [dist (reduce (fn [d edge]
                                    (+ d (get edges (sort edge)))) 0 (partition 2 1 locs))]
                 (max t dist))) 0))))



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-09
  (testing "part one"
    (is (= 117 (part-1 input))))

  (testing "part two"
    (is (= 909 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
