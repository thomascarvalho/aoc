^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.06
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "06" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines))

(def input  (->> (slurp (io/resource "inputs/2023/06.txt")) ;; Load the resource
                 parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "Time:      7  15   30
Distance:  9  40  200"))

;; ## Part 1
(defn part-1
  [raw-races]
  (let [[times records] (->> raw-races
                             (map u/parse-out-longs))
        races           (for [[i t] (map-indexed (fn [i t] [i t]) times)]
                          [t (nth records i)])]

    (->>
     (for [[time dist-record] races]
       (->>
        (for [hold  (range 0 (inc time))
              :let  [speed hold
                     rest (- time hold)
                     total-dist (* speed rest)]
              :when (> total-dist dist-record)]
          1)
        (reduce +)))
     (reduce *))
  ;
    ))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [races]

  (let [[time dist-record] (->> races
                                (map #(apply str (re-seq #"\d+" %)))
                                (map parse-long))]
    (->>
     (for [hold  (range 0 (inc time))
           :let  [speed hold
                  rest (- time hold)
                  total-dist (* speed rest)]
           :when (> total-dist dist-record)]
       1)
     (reduce +)))

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-06
  (testing "part one - example"
    (is (= 288 (part-1 input-example))))

  (testing "part one"
      (is (= 2756160 (part-1 input))))

  (testing "part two"
      (is (= 34788142 (part-2 input)))))


{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
(t/render-results (t/run #'test-2023-06))