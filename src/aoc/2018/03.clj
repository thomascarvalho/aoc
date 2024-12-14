^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.03
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "03" "2018"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

(defn parser [s]
  (->> s
       str/split-lines
       (mapv (fn [claim]
               (->> (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" claim)
                    next
                    (mapv u/parse-int))))))
;; # Solution
;;
;; First things first, let's load our input and parse it
(def input (-> (slurp (io/resource "inputs/2018/03.txt"))
               parser)) ;; Load the resource
                                         ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}



;;  Example
(def input-example (parser "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2"))

;; ## Part 1
(defn part-1
  [input]
  (let [claims input]
    (->>
     claims
     (reduce (fn [cells [claim-id margin-left margin-top width height]]
               (concat cells (for [x (range margin-left (+ margin-left width))
                                   y (range margin-top (+ margin-top height))]
                               [x y])))
             [])
     frequencies
     (filter #(> (second %) 1))
     count)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input]
  (let [claims (->> input
                    (reduce (fn [m [claim-id margin-left margin-top width height]]
                              (assoc m claim-id (for [x (range margin-left (+ margin-left width))
                                                      y (range margin-top (+ margin-top height))]
                                                  [x y]))) {}))

        non-overlaped-cells (->>
                             (vals claims)
                             (apply concat)
                             frequencies
                             (filter #(= (second %) 1))
                             (map first)
                             set)]
    (->> claims
         (filter (fn [[_ cells]]
                   (every? non-overlaped-cells cells)))
         ffirst)))

;; Tests
(deftest test-2018-03
  (testing "part one"
    (is (= 113966 (part-1 input))))

  (testing "part two"
    (is (= 235 (part-2 input)))))
