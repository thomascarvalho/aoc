^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.18
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u] 
            [clojure.string :as str]
            [clojure.test :refer :all]))

;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines))

(def input (->> (slurp (io/resource "inputs/2021/18.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))


(defn explode-first-pair [s])

;; ## Part 1
(defn part-1
  [data]
  data)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2021-18
  (testing "exploding pairs"
    (is (= (explode-first-pair "[[[[[9,8],1],2],3],4]") "[[[[0,9],2],3],4]")))
  #_(testing "part one"
     (is (= 1 (part-1 input))))

  #_(testing "part two"
     (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

