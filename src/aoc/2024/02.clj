(ns aoc.2024.02
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [medley.core :as m]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Solution
(defn parser [data]
  (->> data
       u/to-lines
       (mapv (fn [line]
               (mapv parse-long (str/split line #" "))))))

(def input (->> (slurp (io/resource "inputs/2024/02.txt"))
                parser))

;;  Example
(def input-example (parser "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"))

(defn report-safe? [levels]
  (let [[n1 n2] levels
        check-fn (if (< n1 n2) + -)]
    (reduce
     (fn [_ [n1 n2]]
       (if (or (= (check-fn n1 1) n2)
               (= (check-fn n1 2) n2)
               (= (check-fn n1 3) n2))
         true
         (reduced false)))
     true
     (partition 2 1 levels))))

;; ## Part 1
(defn part-1
  [data]
  (let [reports data]
    (reduce
     (fn [safe-reports-count report]
       (+ safe-reports-count (if (report-safe? report) 1 0)))
     0
     reports)))

;; ## Part 2
(defn part-2
  [data]
  (->> data
       (reduce (fn [total-safes report]
                 (if (or (report-safe? report)
                         (->> (m/indexed report)
                              (reduce (fn [_ [i]]
                                        (when (report-safe? (u/vec-remove i report))
                                          (reduced true)))
                                      false)))
                   (inc total-safes)
                   total-safes))
               0)))

;; # Tests
(deftest test-2024-02
  (testing "part one"
    (is (= 407 (part-1 input))))

  (testing "part two"
    (is (= 459 (part-2 input)))))

