^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.14
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (let [[template raw-rules] (str/split data #"\n\n")

        rules-map            (->> raw-rules
                                  str/split-lines
                                  (map #(re-find #"(\w+) -> (\w)" %))
                                  (map (fn [[_ pair insert]]
                                         (hash-map (into [] (char-array pair)) (first (char-array insert)))))
                                  (apply merge))]


    [template rules-map]))

(def input (->> (slurp (io/resource "inputs/2021/14.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"))

;; ## Part 1
(defn part-1
  [[initial-template rules-map]]

  (let [final-template (loop [template initial-template
                              step     0]
                         (if (= step 10)
                           template
                           (let [new-template (atom [])]
                             (doseq [pair (partition 2 1 nil template)
                                     :let [insert (get rules-map (into [] pair))]]
                               (if insert
                                 (swap! new-template conj (first pair) insert)
                                 (swap! new-template conj (first pair) (second pair))))
                             (recur
                              (->> @new-template
                                   (remove nil?)
                                   str/join)
                              (inc step)))))
        freqs          (->>  final-template
                             frequencies
                             (sort-by second)
                             (map second))]
    (- (last freqs) (first freqs))))
  ;
  

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [[initial-template rules-map]]
  (let [final-template (loop [template initial-template
                              step     0]
                         (if (= step 40)
                           template
                           (let [new-template (atom [])]
                             (doseq [pair (partition 2 1 nil template)
                                     :let [insert (get rules-map (into [] pair))]]
                               (if insert
                                 (swap! new-template conj (first pair) insert)
                                 (swap! new-template conj (first pair) (second pair))))
                             (recur
                              (->> @new-template
                                   (remove nil?)
                                   str/join)
                              (inc step)))))
        freqs          (->>  final-template
                             frequencies
                             (sort-by second)
                             (map second))]
    (- (last freqs) (first freqs))))
  ;
  

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2021-14
  (testing "part one"
    (is (= 2602 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

