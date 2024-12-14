^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.15
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(def a-factor 16807)
(def b-factor 48271)

(defn generate-value [start factor]
  (rem (* start factor) 2147483647))


(defn parser [data]
  (->> data
       u/parse-out-longs))

(def input (->> (slurp (io/resource "inputs/2017/15.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

(defn count-matching-pairs [pairs]
  (->> pairs
       (reduce (fn [total pair]
                 (let [[a2 b2]
                       (map #(let [s          %
                                   bits       (Integer/toString s 2)
                                   pad        (- 16 (count bits))
                                   full       (if (> pad 0) (str (str/join "" (repeat pad "0")) bits) bits)
                                   count-full (count full)]
                               (subs full (- count-full 16) count-full))
                            pair)]


                   (if (= a2 b2)
                     (inc total)
                     total))) 0)))

;; ## Part 1
(defn part-1
  [[a-start b-start]]
  (let [dec-pairs (drop 1 (take 40000000
                                (iterate (fn [[prev-a prev-b]]
                                           [(generate-value prev-a a-factor)
                                            (generate-value prev-b b-factor)]) [a-start b-start])))]

    (count-matching-pairs dec-pairs)))


(let [[a-start b-start] input
      dec-pairs         (drop 1 (take 5000000
                                      (iterate (fn [[prev-a prev-b]]
                                                 [(loop [a (generate-value prev-a a-factor)]
                                                    (if (zero? (mod a 4))
                                                      a
                                                      (recur (generate-value a a-factor))))
                                                  (loop [b (generate-value prev-b b-factor)]
                                                    (if (zero? (mod b 8))
                                                      b
                                                      (recur (generate-value b b-factor))))]) [a-start b-start])))]

  (count-matching-pairs dec-pairs))
;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [[a-start b-start] data
        dec-pairs         (drop 1 (take 5000000
                                        (iterate (fn [[prev-a prev-b]]
                                                   [(loop [a (generate-value prev-a a-factor)]
                                                      (if (zero? (mod a 4))
                                                        a
                                                        (recur (generate-value a a-factor))))
                                                    (loop [b (generate-value prev-b b-factor)]
                                                      (if (zero? (mod b 8))
                                                        b
                                                        (recur (generate-value b b-factor))))]) [a-start b-start])))]

    (count-matching-pairs dec-pairs)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-15
  (testing "part one"
    (is (= 567 (part-1 input))))

  (testing "part two"
    (is (= 323 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

