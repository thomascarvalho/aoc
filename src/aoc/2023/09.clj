^{:nextjournal.clerk/visibility :hide-ns
  :nextjournal.clerk/no-cache   true}
(ns aoc.2023.09
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [util :as u]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "09" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map u/parse-out-longs)))

(def input (->> (slurp (io/resource "inputs/2023/09.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"))

;; ## Part 1
(defn part-1
  [data]

  ;; Quick and dirty solution, but it works, TODO: refactor
  (->>
   (for [nums data
         :let [tree (loop [nums nums
                           all  [nums]]
                      (if (every? zero? nums)
                        (reverse all)
                        (let [res (for [[n1 n2] (partition 2 1 nums)]
                                    (- n2 n1))]
                          (recur
                           res (conj all res)))))]]
     (->>
      (map-indexed (fn [i n] [i n]) tree)
      (reduce (fn [h [i n]]
                (conj h
                      (if (zero? i)
                        (conj n 0)
                        (concat n [(+ (last n) (last (nth h (dec i))))])))) [])))

   (map #(-> % last last))
   (reduce +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (->>
   (for [nums data
         :let [history (loop [nums nums
                              all  [nums]]
                         (if (every? zero? nums)
                           (reverse all)
                           (let [res (for [[n1 n2] (partition 2 1 nums)]
                                       (- n2 n1))]
                             (recur
                              res (conj all res)))))]]
     (->>
      (map-indexed (fn [i n] [i n]) history)
      (reduce (fn [h [i n]]
                (conj h
                      (if (zero? i)
                        (cons 0 n)
                        (conj n (- (first n) (first (nth h (dec i)))))))) [])))

   (map #(-> % last first))
   (reduce +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}
(deftest test-2023-09
  (testing "part one - example"
    (is (= 114 (part-1 input-example))))

  (testing "part one"
    (is (= 1842168671 (part-1 input))))

  (testing "part two - example"
    (is (= 2 (part-2 input-example))))

  (testing "part two"
    (is (= 903 (part-2 input)))))

