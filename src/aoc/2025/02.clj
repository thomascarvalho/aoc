(ns aoc.2025.02
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/02.txt"))))

(def input-example "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defn parser [input]
  (as-> input $
    (str/split-lines $)
    (first $)
    (str/split $ #",")
    (map
     (fn [s]
       (let [[start end] (str/split s #"-")]
         [(biginteger start) (inc (biginteger end))]))
     $)))

(defn part-one
  [data]
  (let [ranges (parser data)]
    (->> ranges 
         ; (take 1)
         (reduce 
           (fn [all-invalid-ids [start end]]
             (->> (reduce
                    (fn [current-invalid-ids n]
                      (if (re-matches #"^(\d+)\1$"  (str n))
                       (conj current-invalid-ids n)
                       current-invalid-ids))
                   [] 
                   (range start end))
                  (apply conj all-invalid-ids)))
           [])
         (apply +))))

(defn part-two
  [data]
  (let [ranges (parser data)]
    (->> ranges 
         ; (take 1)
         (reduce 
           (fn [all-invalid-ids [start end]]
             (->> (reduce
                    (fn [current-invalid-ids n]
                      (if (re-matches #"^(\d+)\1+$"  (str n))
                       (conj current-invalid-ids n)
                       current-invalid-ids))
                   [] 
                   (range start end))
                  (apply conj all-invalid-ids)))
           [])
         (apply +))))

(deftest test-2025-02
  (testing "part one"
    (is (= 1227775554N (part-one input-example)))
    (is (= 5398419778N (part-one input))))

  (testing "part two"
    (is (= 4174379265N (part-two input-example)))
    (is (= 15704845910N (part-two input)))))

