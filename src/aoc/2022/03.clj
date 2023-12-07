(ns aoc.2022.03
  (:require [util :refer [read-from-ns]]
            [clojure.test :refer :all]
            [clojure.string :as cs]
            [clojure.set :as cset]))

(def input (read-from-ns ::x))

(def input-example "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defn decode-rucksack [r]
  (let [length     (count r)
        mid-length (/ length 2)]
    [(take mid-length r) (take-last mid-length r)]))

(defn get-value [c]
  (+
   (mod (int c) 32)
   (if (Character/isUpperCase c)
     26
     0)))

(defn get-intersection-and-value [[r1 r2]]
  (let [c (first (cset/intersection (set r1) (set r2)))]
    (get-value c)))

(defn part-one []
  (let [rucksacks (map decode-rucksack (cs/split-lines input))]
    (->>
     rucksacks
     (map get-intersection-and-value)
     (reduce +))))

(defn part-two []
  (let [rucksacks-group  (partition 3 (cs/split-lines input))]
    (->>
     (map (fn [r]
            (let [[s1 s2 s3] (map #(set (chars (char-array %))) r)]
              (get-value (first (cset/intersection s1 s2 s3))))) rucksacks-group)
     (reduce +))))

(deftest test-2022-03
  (testing "part one"
    (is (= 8298 (part-one))))

  (testing "part two"
    (is (= 2708 (part-two)))))


