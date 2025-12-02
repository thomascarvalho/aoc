(ns aoc.2025.01
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/01.txt"))))

(def DIVISOR 100)

(def input-example "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defn ->op [direction]
  (case direction
    :R +
    :L -))

(defn rotate [current-index [direction offset]]
  (mod
   ((->op direction) current-index offset)
   DIVISOR))

(defn parser [input]
  (->> input
       str/split-lines
       (mapv
        (fn [line]
          [(keyword (subs line 0 1))
           (Integer/parseInt (subs line 1))]))))

(defn part-one
  [data]
  (let [moves (parser data)]
    (->> moves
         (reduce
          (fn [{:keys [current-index] :as m} move]
            (let [new-index (rotate current-index move)]
              (cond-> m
                (zero? new-index) (update :password inc)
                :always (assoc :current-index new-index))))
          {:current-index 50
           :password 0})
         :password)))

(defn rotate-2 [current-index [direction offset]]
  (let [op (->op direction)]
    (loop [offset offset
           current-index current-index
           traversed-zeros 0]
      (if (zero? offset)
        {:index current-index
         :traversed-zeros traversed-zeros}
        (let [i (op current-index 1)
              new-index (cond
                          (= i -1) 99
                          (= i 100) 0
                          :else i)]
          (recur
           (dec offset)
           new-index
           (if (zero? new-index)
             (inc traversed-zeros)
             traversed-zeros)))))))


(defn part-two
  [data]
  (let [moves (parser data)]
    (->> moves
         (reduce
          (fn [{:keys [current-index] :as m} move]
            (let [{:keys [index traversed-zeros]} (rotate-2 current-index move)]
              (-> m
                  (assoc :current-index index)
                  (update :password + traversed-zeros))))
          {:current-index 50
           :password 0})
         :password)))

(deftest test-2025-01
  (testing "part one"
    (is (= 3 (part-one input-example)))
    (is (= 1084 (part-one input))))

  (testing "part two"
    (is (= 6 (part-two input-example)))
    (is (= 6475 (part-two input)))))

