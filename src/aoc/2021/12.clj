^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.12
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.string :as str]
            [loom.alg :as loom-alg]
            [clojure.test :refer :all]))

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map #(str/split % #"-"))))

(def input (->> (slurp (io/resource "inputs/2021/12.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "start-A
start-b
A-c
A-b
b-d
A-end
b-end"))

(defn find-all-paths
  [graph start end]
  (letfn [(is-uppercase? [node]
            (-> node str first java.lang.Character/isUpperCase))
          (find-paths [node path visited]
            (if (= node end)
              [path]
              (apply concat (for [next-node (uber/successors graph node)
                                  :when     (or (is-uppercase? next-node)
                                                (not (contains? visited next-node)))]
                              (find-paths next-node
                                          (conj path next-node)
                                          (conj visited next-node))))))]
    (find-paths start [start] #{start})))

;; ## Part 1
(defn part-1
  [raw-edges]
  (let [g (->
           (uber/graph)
           (uber/add-edges* raw-edges))]

    (uber/pprint g)
    

    (->
     (find-all-paths g "start" "end")
     #_count)))
    ;
    
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
#_(deftest ^:pending test-2021-12
    #_(testing "part one"
       (is (= 1 (part-1 input))))

    #_(testing "part two"
       (is (= 1 (part-2 input)))))

