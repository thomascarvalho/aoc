(ns aoc.2022.06
  (:require [util :refer [read-from-ns]]
            [clojure.test :refer :all]))

(def input (read-from-ns ::x))

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




