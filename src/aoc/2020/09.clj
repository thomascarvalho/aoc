^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2020.09
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map bigint)
       vec))

(def input (->> (slurp (io/resource "inputs/2020/09.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"))

(def input-example-2 (parser "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"))

;; ## Part 1
(defn part-1
  [numbers]
  (let [preamble 25]
    (->>
     (loop [index    preamble
            invalids []]

       (if (< index (count numbers))

         (let [n        (nth numbers index)
               previous (mapv numbers (range (dec index) (- index (inc preamble)) -1))
               valid?   (->>
                         (for [n1    previous
                               n2    previous
                               :when (not= n1 n2)]
                           (+ n1 n2))
                         (filter #(= % n))
                         seq)]
           (recur (inc index) (if valid? invalids (conj invalids n))))
         invalids))
     first
     int)))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [numbers]

  (let [res
        (loop [p 2]
          (let [r (->>
                   (partition p 1 numbers)
                   (filter (fn [g]
                             (let [total (reduce + g)]
                               (= 41682220N #_127N total))))
                   first)]

            (or r (recur (inc p)))))]

    (->
     (+ (reduce min res) (reduce max res))
     int)))
  ;
  



;; Tests
(deftest test-2020-09
  (testing "part one"
    (is (= 41682220 (part-1 input))))

  (testing "part two"
    (is (= 5388976 (part-2 input)))))
