^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.04
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [instaparse parse-int]]))

;; # Solution

(defn parser [data]
  (->> data
       str/split-lines
       (map #(instaparse % "<S> = <'Card'> <SPACE*>? ID <':'> W-NUMS <' |'> MY-NUMS
                            W-NUMS = NUM+
                            MY-NUMS = NUM+
                            <NUM> = <SPACE*>? INT
                            ID = INT
                            INT = #'-?\\d+'
                            SPACE = ' '
                            "
                         {:NUM     parse-int
                          :INT     parse-int
                          :W-NUMS  (fn [& nums]
                                     (set nums))
                          :MY-NUMS (fn [& nums]
                                     (vec nums))}))
       (mapv (fn [game]
               (let [[id winning mine] game
                     inter             (filter winning mine)
                     points            (loop [w-inter inter
                                              p       0]
                                         (if (seq w-inter)
                                           (recur (next w-inter) (if (zero? p) 1 (* p 2)))
                                           p))]

                 (concat game [points inter]))))))

(def input (->> (slurp (io/resource "inputs/2023/04.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

;; ## Part 1
(defn part-1
  [games]
  (->> games
       (map #(nth % 3))
       (reduce +)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; Determine the "score" of a card-- how many of my numbers match the winning
;; numbers. Don't worry about converting that to points, we'll do that in the
;; main loop.
(defn- score-card [[winning mine]]
  (count (filter winning mine)))

;; Adjust the vector of `counts` based on how many winning numbers card `n`
;; had. Note that each "prize" card is increased by `x`, where x is the number
;; of instances of card n that we have.
(defn- adjust [counts n wins]
  (let [x (counts n)]
    (loop [[m & ms] (range wins)
           counts   counts]
      (cond
        (nil? m) counts
        :else    (recur ms (update counts (+ n m 1) + x))))))

;; Count the cards. Start at the first card and go forward. No need for any
;; backtracking due to the nature of the problem (all additions are forward).
(defn- count-cards [cards]
  (let [counts (vec (repeat (count cards) 1))]
    (loop [[n & ns] (range (count cards))
           counts   counts]
      (cond
        (nil? n) (reduce + counts)
        :else    (recur ns (adjust counts n (score-card (cards n))))))))

(defn part-2
  [games]
  (->>
   (mapv (fn [[_ winning mine]]
           [winning mine]) games)
   count-cards))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-04
  (testing "part one"
    (is (= 25651 (part-1 input))))

  (testing "part two"
    (is (= 19499881 (part-2 input)))))

