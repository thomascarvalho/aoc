^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.17
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.core.matrix :as ma]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "17" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       (u/to-matrix (fn [s] (-> s str parse-long)))
       ma/matrix))

(def input (->> (slurp (io/resource "inputs/2023/17.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"))

(defn mget [m [y x]]
  (try
    (ma/mget m y x)
    (catch Exception _e nil)))

(def targets [[1 0] [-1 0] [0 1] [0 -1]])

(defn to-target-coords [[y x] [ty tx]]
  [(+ y ty) (+ x tx)])

;; ## Part 1
(defn part-1
  [m]
  (let [max-y (dec (count m))
        max-x (dec (count (first m)))
        edges (for [current (ma/index-seq m)
                    target  (map #(to-target-coords current %) targets)
                    :let    [v (mget m target)]
                    :when   v]
                [current target v])
        g     (->
               (uber/digraph)
               (uber/add-edges* edges))]

    (alg/shortest-path g {:start-node  [0 0]
                          :end-node    [max-y max-x]
                          :cost-attr   :weight
                          ;; :traverse    true
                          :edge-filter (fn [e]
                                         (println e)
                                         e)
                          ;; :node-filter (fn [n]
                          ;;                (println n)
                          ;;                n
                          ;;                )
                          })




    #_(for [x (range 0 max-x)
            y (range 0 max-y)]))

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
(deftest test-2023-17
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
(part-1 input-example)