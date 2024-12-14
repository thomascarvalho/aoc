^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.test :refer :all]))


;; # Solution

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

(defn card-rank
  ([c] (card-rank 11 c))
  ([j-value c]
   (case c
     \A 14
     \K 13
     \Q 12
     \J j-value
     \T 10
     (parse-long (str c)))))

(defn hand-rank [hand]
  (let [freqs (->> hand
                   frequencies
                   vals
                   (sort >)
                   vec)]
    (apply conj
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

;; ## Part 1
(defn part-1
  [data]
  (->>
   data
   (map (fn [[hand bid]]
          [(hand-rank hand) hand bid]))
   (sort-by first)
   (map-indexed (fn [idx [_ _ bid]] (* (inc idx) bid)))
   (reduce +)))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn freqs-to-vec [freqs]
  (->> freqs
       vals
       (sort >)
       vec))

(defn rank-p2 [hand]
  (let [freqs-all       (frequencies hand)
        freqs-no-jokers (freqs-to-vec (dissoc freqs-all \J))
        labels          (count freqs-no-jokers)
        j               (or (get freqs-all \J) 0)
        a               (or (first freqs-no-jokers) 0)
        m               (+ j a)]

    (cond
      (= 5 m) 7
      (= 4 m) 6
      (and (= 3 m) (= labels 2)) 5
      (= 3 m) 4
      (and (= labels 3) (= j 0)) 3
      (= 2 m) 2
      :else 1)))

(defn compare-hands [h1 h2]
  (let [r1 (rank-p2 h1)
        r2 (rank-p2 h2)]
    (cond
      (< r1 r2) -1
      (> r1 r2) 1
      :else (compare (mapv (partial card-rank 0) h1) (mapv (partial card-rank 0) h2)))))
  

(defn part-2
  [data]
  (->>
   data
   #_(map (fn [[hand bid]]
           [(hand-rank-p2 hand) hand bid]))
   (sort-by first compare-hands)
   (map-indexed (fn [idx [_ bid]] (* (inc idx) bid)))
   (reduce +)))
  ;
  
;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-07
  (testing "part one - example"
    (is (= 6440 (part-1 input-example))))

  (testing "part one"
    (is (= 253954294 (part-1 input))))

  (testing "part two - example"
    (is (= 5905 (part-2 input-example))))

  (testing "part two"
      (is (= 254837398 (part-2 input)))))

