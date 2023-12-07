^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.01
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [util :as u]
            [nextjournal.clerk :as clerk]
            [clojure.string :as cs]
            [clojure.test :refer :all]))



;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "01" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

(def input (->> (slurp (io/resource "inputs/2022/01.txt"))))

(defn part-one []
  (let [elves  (cs/split input #"\n\n")]
    (->> elves
         ;; get calories for each elves, parse and sum
         (map (fn [r] (reduce + (map #(Integer/parseInt %) (cs/split r #"\n")))))
         ;; sort desc and get first
         (sort >)
         first)))

(defn part-two []
  (let [elves  (cs/split input #"\n\n")]
    (->> elves
         ;; get calories for each elves, parse and sum
         (map (fn [r] (reduce + (map #(Integer/parseInt %) (cs/split r #"\n")))))
          ;; sort desc
         (sort >)
         ;; take first 3 elements and sum
         (take 3)
         (reduce +))))

(deftest test-2022-01
  (testing "part one"
    (is (= 69693 (part-one))))

  (testing "part two"
    (is (= 200945 (part-two)))))


