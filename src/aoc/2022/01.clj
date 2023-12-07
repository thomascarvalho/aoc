(ns aoc.2022.01
  (:require [util :as u]
            [clojure.string :as cs]
            [clojure.test :refer :all]))

(def input (u/read-from-ns ::x))

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


