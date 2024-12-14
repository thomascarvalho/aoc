^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.24
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution

(defn parser [data]
  (->> data
       u/parse-out-longs))

(def input (->> (slurp (io/resource "inputs/2015/24.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "11
9
10
8
2
7
5
4
3
1"))

(defn int-partition [num size]
  (let [f (int (Math/pow 10 size))]
    (loop [n num
           l ()]
      (if (zero? n)
        (vec l)
        (recur (int (/ n f)) (conj l (mod n f)))))))
(defn split-nums [nums factor] (vec (map #(int-partition % factor) nums)))


(let [data        input-example
      total       (reduce + data)
      group-total (/ total 3)]

  (split-nums [20 10 5 59999] 3)


  group-total)
  ;
  

;; ## Part 1
(defn part-1
  [data]
  data)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input])

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
#_(deftest test-2015-24
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

