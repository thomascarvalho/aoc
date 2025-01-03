^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.01
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution
(defn parser [data]
  (->> data
       str/split-lines))

(def input (->> (slurp (io/resource "inputs/2023/01.txt"))
                parser))

;;  Example

{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}
(def input-example (parser "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"))

(def input-example-2 (parser "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"))

;; ## Part 1
(defn part-1
  [line-digits]
  (->> line-digits
       (map #(re-seq #"\d" %))
       (map (fn [nums]
              (->
               (str (first nums) (last nums))
               parse-long)))
       (reduce +)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(def mapping
  {"one"   1
   "two"   2
   "three" 3
   "four"  4
   "five"  5
   "six"   6
   "seven" 7
   "eight" 8
   "nine"  9})

(def all-numbers
  (reduce-kv (fn [arr k v]
               (conj arr k (str v))) [] mapping))


(defn parse-n [[_i digits-or-letters]]
  (get mapping digits-or-letters digits-or-letters))

(defn get-indexes [s value]
  (loop [index   (str/index-of s value)
         indexes []]
    (if index
      (recur (str/index-of s value (+ index (count value))) (conj indexes index))
      indexes)))

(defn part-2
  [lines]
  (->> lines
       (map #(mapcat concat
                     (for [n     all-numbers
                           :let  [indexes (get-indexes % n)]
                           :when (seq indexes)]
                       (map (fn [i] [i n]) indexes))))
       (map #(sort-by first %))
       (map (fn [nums]
              (let [s (str (parse-n (first nums)) (parse-n (last nums)))]
                (parse-long s))))
       (reduce +)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(deftest test-2023-01
  (testing "part one"
    (is (= 54644 (part-1 input))))
  
  (testing "part two - example"
    (is (= 281 (part-2 input-example-2))))

  (testing "part two"
    (is (= 53348 (part-2 input)))))

