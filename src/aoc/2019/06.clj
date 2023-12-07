^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2019.06
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "06" "2019"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map #(str/split % #"\)"))))

(def input (->> (slurp (io/resource "inputs/2019/06.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"))

(def input-example-2 (parser "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN"))

;; ## Part 1
(defn part-1
  [input]
  (let [g (-> (uber/digraph)
              (uber/add-edges* input))]
    (->>
     (for [n (uber/nodes g)]
       (loop [node             n
              all-predecessors []]
         (let [predecessors (uber/predecessors g node)]
           (if predecessors
             (recur (first predecessors) (concat all-predecessors predecessors))
             all-predecessors))))
     flatten
     count))
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
  (let [g (-> (uber/graph)
              (uber/add-edges* input))]
    (-> (alg/shortest-path g "YOU" "SAN")
        :cost
        (- 2)))
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2019-06
  (testing "part one"
    (is (= 278744 (part-1 input))))

  (testing "part two"
    (is (= 475 (part-2 input)))))