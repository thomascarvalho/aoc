^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.10
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "10" "2017"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/parse-out-longs))

(def input (->> (slurp (io/resource "inputs/2017/10.txt")) ;; Load the resource
                ))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

(defn get-num [nums idx]
  (get nums (mod idx (count nums))))

(defn set-num [nums idx v]
  (assoc nums (mod idx (count nums)) v))

(defn get-range [nums start length]
  (map #(get-num nums %) (range start (+ start length))))

;;  Example
(def input-example (parser "3, 4, 1, 5"))

;; ## Part 1
(defn part-1
  [data]
  (let [lengths data
        ;; nums    [0, 1, 2, 3, 4]
        nums    (into [] (range 0 256))

        [a b]   (loop [nums               nums
                       idx                0
                       skip               0
                       [l & next-lengths] lengths]
                  (if l
                    (let [reversed-sub (into [] (reverse (get-range nums idx l)))]
                      (recur
                       (reduce (fn [nums i]
                                 (set-num nums (+ idx i) (get reversed-sub i))) nums (range 0 l))
                       (+ idx l skip)
                       (inc skip)
                       next-lengths))
                    nums))]
    (* a b)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-10
  (testing "part one"
    (is (= 1935 (part-1 (parser input)))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

(int \,)

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/test-render #'test-2017-10)

(-> input
    (str/replace  #"\n" "")
    char-array)