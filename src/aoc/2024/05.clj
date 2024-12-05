(ns aoc.2024.05
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :refer [test-render]]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.math.numeric-tower :as math]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "05" "2024"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
(defn parser [data]
  (let [[raw-rules raw-updates] (str/split data #"\n\n")]
    {:rules (reduce (fn [m line]
                      (let [[n1 n2] (u/parse-out-longs line)]
                        (update m n1 conj n2)))
                    {}
                    (u/to-lines raw-rules))
     :updates (mapv (fn [line]
                      (mapv parse-long (str/split line #",")))
                    (u/to-lines raw-updates))}))

(def input (->> (slurp (io/resource "inputs/2024/05.txt"))
                parser))

;;  Example
(def input-example (parser "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"))

(defn get-output-value [all-items]
  (->> all-items
       (map (fn [items]
              (get items (math/round (math/floor (/ (count items) 2))))))
       (reduce +)))

(defn assoc-errors [rules current-updates]
  (reduce (fn [{:keys [items] :as o} n]
            (let [errors (reduce
                          (fn [n-errors p]
                            (if (some #(= % p) (get rules n))
                              (conj n-errors [[(.indexOf current-updates n) n] [(.indexOf current-updates p) p]])
                              n-errors))
                          []
                          items)]
              (cond-> o
                (seq errors) (update :errors concat errors)
                :always (update :items conj n))))
          {:items []
           :errors []}
          current-updates))

;; ## Part 1
(defn part-1
  [data]
  (let [{:keys [rules updates]} data]
    (->> updates
         (map #(assoc-errors rules %))
         (filter #(= (count (:errors %)) 0))
         (map :items)
         get-output-value)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  (let [{:keys [rules updates]} data
        max-loop 500]
    (->> updates
         (map #(assoc-errors rules %))
         (filter #(pos? (count (:errors %))))
         (map (fn [updates]
                (loop [{:keys [items errors] :as updates} updates
                       step 0]
                  (let [[first-error] errors
                        [[i1 v1] [i2 v2]] first-error]
                    (if (or (not first-error)
                            (= step max-loop))
                      updates
                      (recur
                       (assoc-errors rules (-> items
                                               (assoc i2 v1)
                                               (assoc i1 v2)))
                       (inc step)))))))
         (map :items)
         get-output-value)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-05
  (testing "part one"
    (is (= 4637 (part-1 input))))

  (testing "part two"
    (is (= 6370 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(test-render #'test-2024-05)
