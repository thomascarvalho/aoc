^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.01
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all

                                :result :show]))

;; # Solution

(defn parser [data]
  (-> data
      str/split-lines
      first
      (str/split #"")))

(def input (->> (slurp (io/resource "inputs/2015/01.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "))((((("))

;; ## Part 1
(defn part-1
  [initial-actions]
  (loop [actions initial-actions
         floor   0]
    (if (seq actions)
      (recur (next actions) (case (first actions)
                              "(" (inc floor)
                              ")" (dec floor)))
      floor)))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [initial-actions]
  (loop [actions initial-actions
         floor   0
         index   0]
    (if (and (not (neg? floor)) (seq actions))
      (recur
       (next actions)
       (case (first actions)
         "(" (inc floor)
         ")" (dec floor))
       (inc index))
      index)))



;; Tests
(deftest test-2015-01
  (testing "part one"
    (is (= 3 (part-1 input-example))))

  (testing "part one"
    (is (= 74 (part-1 input))))

  (testing "part two"
      (is (= 1795 (part-2 input)))))
