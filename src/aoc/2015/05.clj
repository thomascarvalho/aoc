^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.05
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.test :refer :all]))


;; # Solution

(defn parser [data]
  (->> data
       u/to-lines))

(def input (->> (slurp (io/resource "inputs/2015/05.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "ugknbfddgicrmopn
aaa
jchzalrnumimnmhp
haegwjzuvuyypxyu
dvszwmarrgswjxmb"))

(def input-example-2 ["qjhvhtzxzqqjkmpb"
                      "xxyxx"
                      "uurcxstgmygtbstg"
                      "ieodomkazucvgmuy"])



;; ## Part 1
(defn part-1
  [data]
  (->> data
       (filter
        (fn [s]
          (and
           (>= (count (re-seq #"[aeiou]" s)) 3)
           (reduce (fn [_ [a b]]
                     (if (= a b)
                       (reduced true)
                       false)) false (partition 2 1 s))
           (reduce (fn [_ pair]
                     (if (some #{pair} [[\a \b] [\c \d] [\p \q] [\x \y]])
                       (reduced false)
                       true)) true (partition 2 1 s)))))
       count))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (->> data
       (filter
        (fn [s]
          (let [rule-1? (seq (re-seq #"(.{2})(.+)?\1{1,}" s))
                rule-2? (reduce (fn [_ [a b c]]
                                  (if (and (= a c) (not= a b))
                                    (reduced true)
                                    false)) false (partition 3 1 s))]
            (and
             rule-1?
             rule-2?))))
       count))



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-05
  (testing "part one"
    (is (= 258 (part-1 input))))

  (testing "part two"
    (is (= 53 (part-2 input)))))

