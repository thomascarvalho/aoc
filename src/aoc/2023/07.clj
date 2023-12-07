^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "07" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map #(re-seq #"\w+" %))
       (map (fn [[hand bid]]
              [hand (parse-long bid)]))))

(def input (->> (slurp (io/resource "inputs/2023/07.txt")) ;; Load the resource
                parser))                             ;; Split into lines@
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"))

(defn card-rank [c]
  (case c
    \A 14
    \K 13
    \Q 12
    \J 11
    \T 10
    (parse-long (str c))))

(defn hand-rank [hand]
  (let [freqs (->> hand
                   frequencies
                   vals
                   (sort >)
                   vec)]
    (conj
     [(match freqs
        [5] 7
        [4 1] 6
        [3 2] 5
        [3 1 1] 4
        [2 2 1] 3
        [2 1 1 1] 2
        [1 _ _ _ _] 1
        :else (throw (Exception. "Bad match case")))]
     (mapv card-rank hand))))

(defn part-1
  [data]
  (->>
   data
   (map (fn [[hand bid]]
          [(hand-rank hand) hand bid]))
   (sort-by first)
   (map-indexed (fn [idx [_ _ bid]] (* (inc idx) bid)))
   (reduce +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (->>
   data
   (map (fn [[hand bid]]
          [(hand-rank hand) hand bid]))
   (sort-by first)
   (map-indexed (fn [idx [_ _ bid]] (* (inc idx) bid)))
   (reduce +))
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2023-07
  (testing "part one - example"
    (is (= 6440 (part-1 input-example))))

  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two - example"
      (is (= 5905 (part-2 input-example))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

#_(part-2 input-example)