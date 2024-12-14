^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.17
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "17" "2017"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data u/parse-out-longs first))

(def input (->> (slurp (io/resource "inputs/2017/17.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))

(defn insert [v i e] (vec (concat (take i v) [e] (drop i v))))


;; ## Part 1
(defn part-1
  [data]
  (let [step   data
        target 2017
        nums   (loop [nb-inserts 0
                      pos        0
                      nums       [0]]
                 (if (= nb-inserts target)
                   nums
                   (let [new-pos (inc (mod (+ step pos) (count nums)))]
                     (recur
                      (inc nb-inserts)
                      new-pos
                      (insert nums new-pos (inc nb-inserts))))))]
    (as-> nums $
      (.indexOf $ target)
      (inc $)
      (nth nums $))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}


#_(let [step   input
        target 50000000
        nums   (loop [nb-inserts 0
                      pos        0
                      nums       [0]]
                 (if (= nb-inserts target)
                   nums
                   (let [new-pos (inc (mod (+ step pos) (count nums)))]
                     (recur
                      (inc nb-inserts)
                      new-pos
                      (insert nums new-pos (inc nb-inserts))))))]
    (as-> nums $
      (.indexOf $ 0)
      (inc $)
      (nth nums $)))

(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-17
  (testing "part one"
    (is (= 1506 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

