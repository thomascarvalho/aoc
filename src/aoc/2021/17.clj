^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.17
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.core.matrix :as m]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines))

(def input (->> (slurp (io/resource "inputs/2021/17.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))

;; ## Part 1
(defn part-1
  [data]
  data)
  ;
  


;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input])
  
  ;
  



;; Tests
#_(deftest ^:pending test-2021-17
    #_(testing "part one"
       (is (= 1 (part-1 input))))

    #_(testing "part two"
       (is (= 1 (part-2 input)))))

