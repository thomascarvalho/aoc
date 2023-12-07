^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.04
  {:nextjournal.clerk/toc true}
  (:require [util :as u :refer [parse-int]]
            [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.test :refer :all]
            [clojure.string :as cs]
            [clojure.set :as cset]))


;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "04" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

(def input (->> (slurp (io/resource "inputs/2022/04.txt"))))

(def input-example "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn decode-range [r]
  (let [[n1 n2] (map parse-int (cs/split r #"-"))]
    (apply sorted-set (range n1 (inc n2)))))

(defn parser [data]
  (->> data
       cs/split-lines
       (map #(cs/split % #","))
       (map #(map decode-range %))))

(defn part-one []
  (let [ranges (parser input)]
    (->> ranges
         (map (fn [[s1 s2]]
                (let [inter (cset/intersection s1 s2)]
                  (if (or (= (count inter) (count s1))
                          (= (count inter) (count s2)))
                    1
                    0))))
         (reduce +))))

(defn part-two []
  (let [ranges (parser input)]
    (->> ranges
         (map (fn [[s1 s2]]
                (let [inter (cset/intersection s1 s2)]
                  (if (seq inter) 1 0))))
         (reduce +))))

(deftest test-2022-04
  (testing "part one"
    (is (= 433 (part-one))))

  (testing "part two"
    (is (= 852 (part-two)))))






