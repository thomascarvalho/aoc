(ns aoc.2024.03
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :refer [test-render]]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "03" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
(defn parser [data]
  (->> data
       str/split-lines
       (str/join "")))

(def input (->> (slurp (io/resource "inputs/2024/03.txt"))
                parser))

;;  Example
(def input-example-part-1 (parser "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"))

;; ## Part 1
(defn part-1
  [data]
  (->> (re-seq #"mul\(\d+,\d+\)" data)
       (map u/parse-out-longs)
       (reduce (fn [total [n1 n2]]
                 (+ total (* n1 n2)))
               0)))

;; ## Part 2
(def input-example-part-2 (parser "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  (->>
    (-> data
        (str/replace #"don't\(\)(.*?)?do\(\)" "")
        (str/replace #"don't\(\).*" ""))
   (re-seq #"mul\(\d+,\d+\)")
   (map u/parse-out-longs)
   (reduce (fn [total [n1 n2]]
             (+ total (* n1 n2)))
           0)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-03
  (testing "part one"
    (is (= 180233229 (part-1 input))))

  (testing "part two"
    (is (= 95411583 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(test-render #'test-2024-03)
