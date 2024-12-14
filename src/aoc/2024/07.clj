(ns aoc.2024.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.test :refer [deftest is testing]]))

;; # Solution
(defn parser [data]
  (->> data
       u/to-lines
       (mapv u/parse-out-longs)))

(def input (->> (slurp (io/resource "inputs/2024/07.txt"))
                parser))

;;  Example
(def input-example (parser "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"))

;; Logic
(defn solve-equation [[expected-result & numbers] operators]
  (loop [temp-result #{(first numbers)}
         numbers (rest numbers)]
    (if-let [n2 (first numbers)]
      (recur
       (into #{} (mapcat (fn [n1] (map #(% n1 n2) operators)) temp-result))
       (rest numbers))
      (temp-result expected-result))))

(defn solve-equations [equations operators]
  (->> equations
       (reduce (fn [total equation]
                 (if-let [r (solve-equation equation operators)]
                   (+ total r)
                   total))
               0)))

;; ## Part 1
(defn part-1
  [data]
  (solve-equations data [+ *]))

;; ## Part 2
(defn concat-numbers [n1 n2]
  (bigint (str n1 n2)))

(defn part-2
  [data]
  (solve-equations data [+ * concat-numbers]))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-07
  (testing "part one"
    (is (= 3749 (part-1 input-example)))
    (is (= 538191549061 (part-1 input))))

  (testing "part two"
    (is (= 11387 (part-2 input-example)))
    (is (= 34612812972206 (part-2 input)))))
