(ns aoc.YEAR.DAY
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u] 
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "DAY" "YEAR"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Parser
(defn parser [data]
  (->> data
       u/to-lines))

;; Inputs
(def input (->> (slurp (io/resource "inputs/YEAR/DAY.txt"))
                parser))

(def input-example (parser ""))

;; Logic

;; ## Part 1
(defn part-1
  [data]
  data)

;; ## Part 2
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-YEAR-DAY
  #_(testing "part one"
     (is (= 1 (part-1 input))))

  #_(testing "part two"
     (is (= 1 (part-2 input)))))

