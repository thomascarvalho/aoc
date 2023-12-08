^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.YEAR.DAY
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "DAY" "YEAR"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines))

(def input (->> (slurp (io/resource "inputs/YEAR/DAY.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))

;; ## Part 1
(defn part-1
  [data]
  data
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input]
  
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-YEAR-DAY
  (testing "part one"
    (is (= 1 (part-1 input))))

  (testing "part two"
    (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
(t/render-results (t/run #'test-YEAR-DAY))

(part-1 input-example)