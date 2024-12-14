(ns aoc.2024.11
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (->> data
       u/parse-out-longs
       (into [])))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/11.txt"))
                parser))

(def input-example (parser "0 1 10 99 999"))

;; Logic

(defn number-length
  "Calcule la longueur d'un nombre"
  ^long [^long n]
  (if (zero? n)
    1
    (inc (int (Math/log10 (double n))))))

(defn split-number
  "Divise un nombre en deux parties égales"
  ^clojure.lang.PersistentVector [^long n]
  (let [s (str n)
        len (count s)
        half (quot len 2)]
    [(Long/parseLong (subs s 0 half))
     (Long/parseLong (subs s half))]))

(def  multiplier 2024)

(defn blink [stones]
  (persistent! (reduce
                  (fn [new-stones stone]
                    (cond
                      (zero? stone)
                      (conj! new-stones 1)

                      (even? (number-length stone))
                      (let [[n1 n2] (split-number stone)]
                        (-> new-stones
                            (conj! n1)
                            (conj! n2)))

                      :else (conj! new-stones (* stone multiplier))))
                  (transient [])
                  stones)))

(defn process [stones expected-times]
  (loop [stones stones
         times 0]
    (if (>= times expected-times)
      (count stones)
      (recur (blink stones)
             (inc times)))))

;; ## Part 1
(defn part-1
  [data]
  (process data 25))

;; ## Part 2
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-11
  (testing "logics"
    (is (= (blink [0 1 10 99 999]) [1 2024 1 0 9 9 2021976])))
  (testing "part one"
    (is (= 184927 (part-1 input))))
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

(time (process input 25))

