^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.08
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [test-util :as t]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "08" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (let [lines       (->> data
                         str/split-lines)
        grabs       (->> lines
                         first
                         (map #(case %
                                 \R second
                                 \L first)))
        network-map (->> lines
                         (drop 2)
                         (reduce (fn [acc l]
                                   (let [[_ node choice-1 choice-2] (re-find #"(\w+) = \((\w+), (\w+)\)" l)]
                                     (assoc acc node [choice-1 choice-2]))) {}))]
    [grabs network-map]))

(def input (->> (slurp (io/resource "inputs/2023/08.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;; ## Part 1
(defn part-1
  [[grabs network-map]]

  (->> (cycle grabs)
       (reduce
        (fn [[current step] grab]
          (if (= current "ZZZ")
            (reduced step)
            [(-> (get network-map current) grab) (inc step)])) ["AAA" 0])))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [[grabs network-map]]
  (let [starting-nodes (->> network-map
                            keys
                            (filter #(str/ends-with? % "A")))]

    (->>
     (for [start starting-nodes]
       (->> (cycle grabs)
            (reduce
             (fn [[current step] grab]
               (if (str/ends-with? current "Z")
                 (reduced step)
                 [(-> (get network-map current) grab) (inc step)])) [start 0])))
     (reduce math/lcm))

    ;; my first naive attempt below, but gives StackOverflowError for puzzle input, need to find the least common multiple -> see above with numeric-tower/lcm solution
    #_(->> (cycle grabs)
           (reduce
            (fn [[current-nodes step] grab]
              (if (every? #(str/ends-with? % "Z") current-nodes)
                (reduced step)
                [(map #(-> (get network-map %) grab) current-nodes) (inc step)])) [starting-nodes 0]))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-08
  (testing "part one - example 1"
    (is (= 2 (part-1 (parser "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")))))

  (testing "part one - example 2"
    (is (= 6 (part-1 (parser "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")))))

  (testing "part one"
    (is (= 12083 (part-1 input))))

  (testing "part two - example"
    (is (= 6 (part-2 (parser "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")))))

  (testing "part two"
    (is (= 13385272668829 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
(t/render-results (t/run #'test-2023-08))