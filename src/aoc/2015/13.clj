^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.13
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.math.combinatorics :as combo]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "13" "2015"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (mapcat #(->> %
                     (re-seq #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).")
                     (map (fn [[_ p1 op happiness p2]]
                            [p1 p2 (case op
                                     "gain" (parse-long happiness)
                                     "lose" (- (parse-long happiness)))]))))))

(def input (->> (slurp (io/resource "inputs/2015/13.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol."))

;; ## Part 1
(defn part-1
  [edges]
  (let [people        (set (map first edges))
        happiness-map (->>
                       (map (fn [[p1 p2 happiness]]
                              (hash-map (str p1 "-" p2) happiness)) edges)
                       (apply merge))]
    (->>
     (combo/permutations people)
     (reduce (fn [r combs]
               (let [full-people (conj (into [] (cons (last combs) combs)) (first combs))
                     parts       (partition 3 1 full-people)
                     t           (reduce (fn [t [p1 p2 p3]]
                                           (+ t (get happiness-map (str p2 "-" p1)) (get happiness-map (str p2 "-" p3)))) 0 parts)]
                 (conj r [combs t]))) [])

     (sort-by second >)
     first
     second))
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [edges]
  (let [people                 (conj (set (map first edges)) "you")
        happiness-map          (->>
                                (map (fn [[p1 p2 happiness]]
                                       (hash-map (str p1 "-" p2) happiness)) edges)
                                (apply merge))
        happiness-map-with-you (reduce (fn [m p]
                                         (assoc m (str "you-" p) 0 (str p "-you") 0)) happiness-map people)]

    (->>
     (combo/permutations people)
     (reduce (fn [r combs]
               (let [full-people (conj (into [] (cons (last combs) combs)) (first combs))
                     parts       (partition 3 1 full-people)
                     t           (reduce (fn [t [p1 p2 p3]]
                                           (+ t (get happiness-map-with-you (str p2 "-" p1)) (get happiness-map-with-you (str p2 "-" p3)))) 0 parts)]
                 (conj r [combs t]))) [])

     (sort-by second >)
     first
     second))

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-13
  (testing "part one"
    (is (= 709 (part-1 input))))

  (testing "part two"
    (is (= 668 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
