(ns aoc.2024.23
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.set :as cset]
            [ubergraph.alg :as alg]
            [ubergraph.core :as uber]
            [clojure.test :refer [deftest is testing]]
            [pathfinding :as pf]))

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
        neighbors (fn [n] (set (uber/neighbors g n)))
        succ (fn [n] (set (uber/successors g n)))]
    (for [n nodes]
      (loop [queue [n]
             connected #{}
             step 0]
        (if (> step 10)
          connected
          (if-let [[current & others] queue]
            (let [current-neighbors (neighbors current)
                  path (set (cons current (apply
                                           cset/union
                                           (for [n current-neighbors]
                                             (filter (fn [n1] (current-neighbors n1)) (neighbors n))))))]
              (apply
               cset/union
               (for [n1 path
                     n2 path
                     :when (and (not= n1 n2)
                                (some #(= % n2) (neighbors n1))
                                (current-neighbors n2)
                                (current-neighbors n1))]
                 #{current n1 n2})))

            connected)))



      #_(for [n1 ["ka" #_nodes]
              :let [n1-n (neighbors n1)]
              n2 n1-n
              :let [n2-n (neighbors n2)]
              (cset/union  n1-n n2-n)])


      #_(loop [start "ka"
               [current & others :as queue] [start]
               path #{start}
               step 0]
          (println queue)
          (if (or (not current) (> step 10))
            path
            (if-let [all-succs (seq (uber/successors g current))]
              (let [new-succs (filter (fn [n] (and ((set (uber/neighbors g start)) n) (not (path n)))) all-succs)]
                (do
                  #_(println new-succs)
                  (recur
                   start
                   (apply conj others new-succs)
                   (apply conj path new-succs)
                   (inc step))))
              path)))

      #_(->> (for [n1 nodes
                   :let [n1-successors (succ n1)]
                   n2 n1-successors
                   :let [n2-successors (succ n2)]
                   n3 n2-successors
                   :when ((succ n3) n1)
                   :let [res #{n1 n2 n3}]
                   :when (some (fn [n] (zero? (.indexOf n "t"))) res)]
               res)
             distinct
             count))))

(part-2 input-example)

;; # Tests
#_{:nextjournal.clerk/visibility {:code   :show
                                  :result :hide}}
#_(deftest test-2024-23
    #_(testing "part one"
        (is (= 1411 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

