^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2024.01
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution
(defn parser [data]
  (->> data
       u/to-lines
       (reduce (fn [[all-n1 all-n2] row]
                 (let [[n1 n2] (str/split row #"   ")]
                   [(conj all-n1 (parse-long n1))
                    (conj all-n2 (parse-long n2))]))
               [[] []])))

(def input (->> (slurp (io/resource "inputs/2024/01.txt")) ;; Load the resource
                parser))                             ;; Split into lines

(def input-example (parser "3   4
4   3
2   5
1   3
3   9
3   3"))
{:nextjournal.clerk/visibility {:result :hide}}

;; ## Part 1
(defn part-1
  [data]
  (let [[l1 l2] (map sort data)]
    (->> (for [[i n1] (map-indexed (fn [i v] [i v]) l1)
               :let [n2 (nth l2 i)]]
           (abs (- n1 n2)))
         (reduce +))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}

(defn part-2
  [data]
  (let [[l1 l2] data
        n2-freqs (frequencies l2)]
    (->> (for [n1 l1
               :let [similarity (get n2-freqs n1 0)]]
           (* n1 similarity))
         (reduce +))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(deftest test-2024-01
  (testing "part one"
    (is (= 1938424 (part-1 input))))

  (testing "part two"
    (is (= 22014209 (part-2 input)))))

