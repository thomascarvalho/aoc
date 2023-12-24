^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.22
  {:nextjournal.clerk/toc true}
  (:refer-clojure
   :exclude [+ - * / zero? compare divide numerator denominator
             infinite? abs ref partial =])
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [emmy.clerk :as ec]
            [util :as u]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]
            #_[emmy.env :as e :refer :all]
            [emmy.mafs :as mafs]
            [emmy.mathbox.plot :as p]
            [emmy.viewer :as ev]))

^{::clerk/visibility {:code   :hide
                      :result :hide}}
(ec/install!)

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "22" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution 
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map #(->> % u/parse-out-longs (partition 3) (into [])))))

(def input (->> (slurp (io/resource "inputs/2023/22.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9"))

;; ## Part 1
(defn part-1
  [bricks]
  (->>  bricks
        (reduce (fn [s [[x1 y1 z1] [x2 y2 z2]]]
                  (conj s (p/line {:coords [[x1 y1 z1] [x2 y2 z2]]
                                   :height 20
                                   :width  20}))) (p/scene)))
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}



(part-1 input-example)

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


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-22
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results