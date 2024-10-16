^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.09
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "09" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines))

(def input (->> (slurp (io/resource "inputs/2018/09.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))







(loop [circle [0]
       current 0
       players (cycle (range 1 10))
       step 0]
  (println current)
  (if (= step 7)
    {:circle circle
     :current current
     :players players}
   (let [player (first players)
         v (count circle)
         [new-current new-circle] (insert-relative circle current 2 v)]
     (println circle new-circle)
     (recur new-circle
            new-current
            (next players)
            (inc step)))))
    
    
  
  


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
(deftest test-2018-09
  (testing "inserting"
    (let [circle [0 1]])
   (is (= [] (add-marble []))))
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                 :result :show}}

#_(t/test-render #'test-2018-09)
