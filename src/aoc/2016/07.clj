^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2016.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u] 
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution

(defn parser [data]
  (->> data
       u/to-lines))

(def input (->> (slurp (io/resource "inputs/2016/07.txt")) ;; Load the resource
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
  



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
#_(deftest test-2016-07
    #_(testing "part one"
       (is (= 1 (part-1 input))))

    #_(testing "part two"
       (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

