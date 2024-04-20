^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.12
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.test :refer :all]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "12" "2017"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map u/parse-out-longs)))

(def input (->> (slurp (io/resource "inputs/2017/12.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"))

(defn get-edges [links]
  (->> links
       (mapcat (fn [[start & links]]
                 (for [l links]
                   [start l])))))

;; ## Part 1
(defn part-1
  [data]
  (let [edges (get-edges data)
        g     (-> (uber/graph)
                  (uber/add-edges* edges))]
    (->>
     (alg/connected-components g)
     (filter #(some #{0} %))
     first
     set
     count)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [edges (get-edges data)
        g     (-> (uber/graph)
                  (uber/add-edges* edges))]
    (->>
     (alg/connected-components g)
     count)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-12
  (testing "part one"
    (is (= 169 (part-1 input))))

  (testing "part two"
    (is (= 179 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(t/test-render #'test-2017-12)