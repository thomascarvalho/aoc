^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.24
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "24" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map
        (fn [row]
          (map
           (fn [cell]
             cell) (char-array row))))))


(def input (->> (slurp (io/resource "inputs/2022/24.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"))

(defn empty-cells [cells]
  (filter #(-> % val (= " ")) cells))

(defn empty-coords [cells]
  (->> cells
       empty-cells
       (map first)))

;; ## Part 1
(defn part-1
  [maze]
  maze
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2022-24
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

#_(part-1 input-example)