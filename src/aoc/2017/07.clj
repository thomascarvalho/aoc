^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (for [row (->> data
                 u/to-lines)]
    (let [[start num & children] (re-seq #"\w+" row)]
      [start (parse-long num) (into [] children)])))

(def input (->> (slurp (io/resource "inputs/2017/07.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"))




;; ## Part 1
(defn part-1
  [data]
  (let [edges (reduce (fn [edges [start num children]]
                        (concat edges (map (fn [end]
                                             [start end]) children))) [] data)]
    (-> (uber/digraph)
        (uber/add-directed-edges* edges)
        (alg/topsort)
        first)))

(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [data            input
        root-node       (part-1 input)
        nodes           (reduce (fn [nodes [node weight]]
                                  (assoc nodes node weight)) {} data)
        edges           (reduce (fn [edges [start num children]]
                                  (concat edges (map (fn [end]
                                                       [start end]) children))) [] data)
        g               (-> (uber/digraph)
                            (uber/add-directed-edges* edges))
        downsort        (reverse (alg/pre-traverse g root-node))
        augmented-nodes (reduce (fn [nodes node]
                                  (if-let [succ (uber/successors g node)]
                                    (assoc nodes node (reduce (fn [t s]
                                                                (+ t (get nodes s))) (get nodes node) succ))
                                    nodes)) nodes downsort)
        res             (->>
                         (map (fn [n]
                                [n (augmented-nodes n)]) (uber/successors g root-node))
                         (group-by second)
                         (sort-by (juxt second count)))

        [[v [[n]]] [t]] res]

    #_(- (nodes n) (- v t))
    #_(augmented-nodes "vgzejbd")

    #_(map (fn [n]
             [n (augmented-nodes n)]) (uber/successors g "kiatxq"))

    (- (nodes "kiatxq") (- v t))))
; 148784 too high
; 1226 to test

  ;
  

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-07
  (testing "part one"
    (is (= "vgzejbd" (part-1 input))))

  (testing "part two"
    (is (= 1226 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

