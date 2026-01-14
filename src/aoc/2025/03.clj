(ns aoc.2025.03
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/03.txt"))))

(def input-example "987654321111111
811111111111119
234234234234278
818181911112111")

(defn parser [input]
  (as-> input $
    (str/split-lines $)
    (map #(str/split % #"") $)
    (mapv #(mapv (fn [s] (Integer/parseInt s)) %) $)))

(defn part-one
  [data]
  (let [banks (parser data)]
    ; banks
    (->> banks
         (reduce
          (fn [t batteries]
            (let [indexed-batteries (map-indexed (fn [i v] [i v]) batteries)
                  r (->> (for [[ia va] indexed-batteries
                               [ib vb] (filter (fn [[i]] (> i ia)) indexed-batteries)]
                           (Integer/parseInt (str/join (map str [va vb]))))
                         (apply max))]
              (if r
                (+ t r)
                t)))
          0))))

(defn part-two
  [data]
  (let [banks (parser data)]
    (->> banks
         (reduce
          (fn [t batteries]
            (let [r (->>
                     (combo/combinations (map-indexed (fn [i v] [i v]) batteries) 12)
                     (transduce
                      (map #(biginteger (str/join (map (comp str second) %))))
                      max
                      0))]
              (if r
                (+ t r)
                t)))
          0))))

#_(deftest test-2025-02
    (testing "part one"
      (is (= 1227775554N (part-one input-example)))
      (is (= 5398419778N (part-one input))))

    (testing "part two"
      (is (= 4174379265N (part-two input-example)))
      (is (= 15704845910N (part-two input)))))

(time (part-two input-example))

; 4092 too low
; 139553732427929N too low
