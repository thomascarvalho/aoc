^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.01
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "01" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}


(def all-numbers ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
                  "1" "2" "3" "4" "5" "6" "7" "8" "9"])

(def mapping {"one"   1
              "two"   2
              "three" 3
              "four"  4
              "five"  5
              "six"   6
              "seven" 7
              "eight" 8
              "nine"  9})

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines))

(defn parse-n [[_i digits-or-letters]]
  (or (get mapping digits-or-letters) digits-or-letters))

(defn get-indexes [s value]
  (loop [index   (str/index-of s value)
         indexes []]
    (if index
      (recur (str/index-of s value (+ index (count value))) (conj indexes index))
      indexes)))

(def input (->> (slurp (io/resource "inputs/2023/01.txt")) ;; Load the resource
                parser))                             ;; Split into lines

{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
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
       (reduce +))
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
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

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-01
  (testing "part one"
    (is (= 54644 (part-1 input))))
  
  (testing "part two - example"
    (is (= 281 (part-2 input-example-2))))

  (testing "part two"
    (is (= 53348 (part-2 input)))))


{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
#_(t/render-results (t/run #'test-2023-01))