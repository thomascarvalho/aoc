^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "07" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it
(defn letter-to-position [letter]
  (let [alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        upper-letter (str/upper-case letter)]
    (if-let [position (str/index-of alphabet upper-letter)]
      (inc position) ; Adding 1 because index is 0-based, but alphabet positions are 1-based
      nil)))
(defn parser [data]
  (->> data
       u/to-lines
       (map (fn [s]
              (let [[_ start end] (re-find #"Step (\w+) must be finished before step (\w+) can begin." s)]
                [start end {:weight (letter-to-position end)}])))))

(def input (->> (slurp (io/resource "inputs/2018/07.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}



;;  Example
(def input-example (parser "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."))

(defn compare-nodes [a b]
  (compare  b  a))









{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-1
  [data]
  (let [g (-> (uber/digraph)
              (uber/add-edges* data))
        nodes (uber/nodes g)
        successors (fn [node] (uber/successors g node))
        predecessors (fn [node] (uber/predecessors g node))
        start-node (first (filter (fn [d] (empty? (uber/predecessors g d))) nodes))
        end-node (first (filter (fn [d] (empty? (successors d))) nodes))]
   (->> (loop [choices (sort (successors start-node))
               path [start-node]
               step 0]
          (if (or (= end-node (last path)) (> step 1000))
            path
            (if-let [node (first (sort (filter (fn [n] (every? (fn [p] (some #(= % p) path)) (predecessors n))) choices)))]
              (recur
                (sort (concat (remove #(= % node) choices) (successors node)))
                (conj path node)
                (inc step))
              path)))
        (str/join ""))))

;; ## Part 2
(defn part-2
  [data]
  data)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2018-07
  (testing "part one"
    (is (= "SCLPAMQVUWNHODRTGYKBJEFXZI" (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/test-render #'test-2018-07)
