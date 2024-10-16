^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2019.02
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "02" "2019"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->>
   (str/split data #",")
   (map parse-int)
   vec))

(def input (->> (slurp (io/resource "inputs/2019/02.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "1,9,10,3,2,3,11,0,99,30,40,50"))

;; ## Part 1
(defn part-1
  [input]
  (->
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
   first))

  ;
  

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]
  (let [[_ n v] (->>
                 (for [noun (range 99)
                       verb (range 99)]
                   (let [address (->
                                  (loop [data        (assoc input 1 noun 2 verb)
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
                                  first)]
                     [address noun verb]))
                 (filter #(= (first %) 19690720))
                 first)]
    (+ (* 100 n) v)))
  ;
  

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; Tests
(deftest test-2019-02
  (testing "part one"
    (is (= 3654868 (part-1 input))))

  (testing "part two"
    (is (= 7014 (part-2 input)))))

