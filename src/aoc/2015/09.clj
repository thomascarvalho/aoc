^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.09
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "09" "2015"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map #(let [[_ from to dist] (re-find #"(\w+) to (\w+) = (\d+)" %)]
               [from to (parse-long dist)]))))

(def input (->> (slurp (io/resource "inputs/2015/09.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141"))



(defn hamiltonian-paths [graph start-node]
  (letfn [(dfs [current-node visited]
            (if (= (count visited) (uber/count-nodes graph))
              [visited]
              (for [next-node (uber/successors graph current-node)
                    :when     (not (contains? (set visited) next-node))]
                (dfs next-node (conj visited next-node)))))]
    (dfs start-node [start-node])))

;; ## Part 1
(defn part-1
  [edges]
  (let [g     (-> (uber/graph)
                  (uber/add-edges* edges))]
    (println edges)
    (->>
     (mapcat #(hamiltonian-paths g %) (uber/nodes g))
     (apply concat)
     (apply concat)
     (map #(vector %
                   (->> (partition 2 1 %)
                        (mapv (fn [d] (->>
                                       edges
                                       (filter (fn [[s e]] (or (= [s e] d) (= [e s] d))))
                                       (map (fn [[_ _ c]] c)))))
                        (mapcat concat)
                        (reduce +))))
     (map (fn [[_ dist]]
            dist))
     (reduce min))))

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


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-09
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

(part-1 input)