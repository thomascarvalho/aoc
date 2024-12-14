^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.18
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "18" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it
(def input (->> (slurp (io/resource "inputs/2022/18.txt")) ;; Load the resource
                str/split-lines
                (map (fn [s] (map parse-int (str/split s #","))))))                             ;; Split into lines


{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Example

(def input-example
  (->>
   "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"
   str/split-lines
   (map (fn [s] (map parse-int (str/split s #","))))))


;; ## Part 1



{:nextjournal.clerk/visibility {:result :hide}}

(defn part-1
  [input]
  input)

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input-example)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]
  nil)

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
#_(deftest test-2018-03
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))
