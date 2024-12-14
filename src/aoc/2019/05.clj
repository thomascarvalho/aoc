^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2019.05
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution
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


#_(let [data (str/split (first input-example) #",")]
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
  

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input])

;; Tests
#_(deftest test-2019-05
    #_(testing "part one"
       (is (= 1 (part-1 input))))

    #_(testing "part two"
       (is (= 1 (part-2 input)))))

