^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.06
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.test :refer :all]))


;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "06" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

(def input (->> (slurp (io/resource "inputs/2022/06.txt"))))

(defn part-one
  ([]
   (part-one {:partition-group 4}))
  ([{:keys [partition-group]}]
   (->>
    (for [[index freqs] (map-indexed (fn [i f] [i (frequencies f)]) (partition partition-group 1 input))]
      (when (= partition-group (count freqs)) (+ index partition-group)))
    (remove nil?)
    first)))

(defn part-two []
  (part-one {:partition-group 14}))

(deftest test-2022-06
  (testing "part one"
    (is (= 1361 (part-one))))

  (testing "part two"
    (is (= 3263 (part-two)))))




