^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.06
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [flatland.ordered.set :as oset]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "06" "2017"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/parse-out-longs
       (into [])))

(def input (->> (slurp (io/resource "inputs/2017/06.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "0 2 7 0"))


(defn get-max-index-and-value [nums]
  (reduce (fn [[c-i c-max] [i v]]
            (if (> v c-max)
              [i v]
              [c-i c-max]))
          [-100 -100] (map-indexed (fn [i n]
                                     [i n]) nums)))


(defn redistribute [nums]
  (let [[max-i max-v] (get-max-index-and-value nums)
        len           (count nums)
        new-nums      (assoc nums max-i 0)]
    (loop [i               (inc max-i)
           to-redistribute max-v
           nums            new-nums]
      (if (zero? to-redistribute)
        nums
        (let [new-i    (mod i len)
              new-nums (update nums new-i inc)]
          (recur (inc i)
                 (dec to-redistribute)
                 new-nums))))))

(defn redistribute-until-duplicate [nums]
  (loop [nums nums
         seen #{nums}]
    (let [new-nums (redistribute nums)]
      (if (seen new-nums)
        [new-nums (count seen)]
        (recur new-nums (conj seen new-nums))))))


;; ## Part 1
(defn part-1
  [data]
  (->
   (redistribute-until-duplicate data)
   second))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (loop [nums data
         seen (oset/ordered-set nums)]
    (let [new-nums (redistribute nums)]
      (if (seen new-nums)
        (let [prev-i (->>
                      (map-indexed (fn [i n]
                                     [i n]) seen)

                      (filter (fn [[i n]]
                                (= n new-nums)))
                      ffirst)]
          (- (count seen) prev-i))
        (recur new-nums (conj seen new-nums))))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-06
  (testing "part one"
    (is (= 14029 (part-1 input))))

  (testing "part two"
    (is (= 2765 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(t/test-render #'test-2017-06)