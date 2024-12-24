(ns aoc.2024.22
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.math.numeric-tower :as nt]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (->> data
       u/parse-out-longs))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/22.txt"))
                parser))

(def input-example (parser "1
10
100
2024"))

;; Logic

(defn mul [secret-number]
  (* secret-number 64))

(defn mix [secret-number n]
  (bit-xor n secret-number))

(defn prune [secret-number]
  (mod secret-number 16777216))


(defn next-secret-number [secret-number]
  (let [temp-secret (->> secret-number
                         mul
                         (mix secret-number)
                         prune)
        n-div (int (nt/floor (/ temp-secret 32)))
        temp-secret (-> n-div
                        (mix temp-secret)
                        prune)]
    (->> (* temp-secret 2048)
         (mix temp-secret)
         prune)))


;; ## Part 1
(defn part-1
  [secret-numbers]
  (reduce
   (fn [total secret-number]
     (+ total (last (take (inc 2000) (iterate next-secret-number secret-number)))))
   0
   secret-numbers))

;; ## Part 2
(defn part-2
  [secret-numbers]
  (->> (reduce
        (fn [arr secret-number]
          (let [numbers (take 2000 (iterate next-secret-number secret-number))
                last-numbers (mapv #(parse-long (str (last (str %)))) numbers)
                seqs (->> (partition 2 1 last-numbers)
                          (mapv (fn [[n1 n2]] (- n2 n1)))
                          (partition 4 1)
                          (map-indexed vector)
                          (mapv (fn [[i se]] [(nth last-numbers (+ i 4)) (vec se)]))
                          (group-by second)
                          (mapv (fn [[se r]] [se (ffirst r)]))
                          (filter (fn [[_ v]] (pos? v))))]
            (persistent!
             (reduce
              (fn [a [se v]]
                (assoc! a se (+ (get a se 0) v)))
              (transient arr)
              seqs))))
        {}
        secret-numbers)
       (reduce
        (fn [max-val [_ sum]]
         (max max-val sum))
        0)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-22
  (testing "part one"
    (is (= 20506453102 (part-1 input))))

  (testing "part two"
    (is (= 2423 (part-2 input)))))

