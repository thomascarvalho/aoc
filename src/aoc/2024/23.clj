(ns aoc.2024.23
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (->> data
       u/to-lines
       (mapv #(conj (str/split % #"-") {:weight 1}))))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/23.txt"))
                parser))

(def input-example (parser "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn"))

;; Logic

;; ## Part 1
(defn part-1
  [edges]
  (let [nodes (reduce
               (fn [nodes [from to]]
                 (conj nodes from to))
               #{}
               edges)
        g (-> (uber/graph)
              (uber/add-edges* edges))
        succ (fn [n] (set (uber/successors g n)))]
    (->> (for [n1 nodes
               :let [n1-successors (succ n1)]
               n2 n1-successors
               :let [n2-successors (succ n2)]
               n3 n2-successors
               :when ((succ n3) n1)
               :let [res #{n1 n2 n3}]
               :when (some (fn [n] (zero? (.indexOf n "t"))) res)]
           res)
         distinct
         count)))

;; ## Part 2
(defn part-2
  [edges]
  (let [nodes (reduce
               (fn [nodes [from to]]
                 (conj nodes from to))
               #{}
               edges)
        g (-> (uber/graph)
              (uber/add-edges* edges))
        neighbors (fn [n] (set (cons n (uber/neighbors g n))))]
    (->> (for [node nodes]
           (let [current-neighbors (neighbors node)]
             (->>
              (for [n current-neighbors
                    :let [n1 (neighbors n)
                          good-n1 (filter (fn [n1'] (some #(= n1' %) current-neighbors)) n1)]]
                (set good-n1))
              (frequencies)
              (sort-by val >)
              first)))
         (sort-by second >)
         ffirst
         sort
         (str/join ","))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-23
  (testing "part one"
    (is (= 1411 (part-1 input))))

  (testing "part two"
    (is (= "aq,bn,ch,dt,gu,ow,pk,qy,tv,us,yx,zg,zu" (part-2 input)))))

