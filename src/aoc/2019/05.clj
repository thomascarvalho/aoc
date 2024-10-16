^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2019.05
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "05" "2019"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines))

(def input (->> (slurp (io/resource "inputs/2019/05.txt")) ;; Load the resource
                str/split-lines))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "1002,4,3,4,33"))

;; ## Part 1
(defn part-1
  [input]
 input)


(let [data (str/split (first input-example) #",")]
  data)
  
#_(->
   (loop [data        (assoc input 1 12 2 2)
          group-index 0]
     (let [groups (partition 4 data)
           g      (nth groups group-index)]
       (if (and g (not= (first g) 99))
         (let [[action input-index-1 input-index-2 output-index] g
               operator                                          (case action
                                                                   1 +
                                                                   2 *)]
           (recur
            (assoc data output-index (operator (nth data input-index-1) (nth data input-index-2)))
            (inc group-index)))
         data)))
   first)
  
  ;
  

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input])
  
  ;
  

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2019-05
  #_(testing "part one"
     (is (= 1 (part-1 input))))

  #_(testing "part two"
     (is (= 1 (part-2 input)))))

(part-1 input-example)
