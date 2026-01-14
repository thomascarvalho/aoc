(ns aoc.2025.06
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [util :as u]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/06.txt"))))

(def input-example "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

(defn parser-p1 [input]
  (->> input
       str/split-lines
       (reduce (fn [acc line]
                 (let [r (str/split line #"[ ]+")]
                   (reduce
                    (fn [m [k v]]
                      (update m k conj (read-string v)))
                    acc
                    (map-indexed vector (remove (fn [s] (= s "")) r)))))
               {})))


(defn part-one
  [data]
  (let [cols (parser-p1 data)]
    (->> cols
         (reduce
          (fn [acc [_ line]]
            (+ acc (eval line)))
          0))))

(defn parser-p2 [input]
  (let [[operators & lines] (->> input
                                 str/split-lines
                                 reverse)
        operators-pos (-> (->> (-> (u/re-pos #"[*+]" (str operators " *")))
                               (partition 2 1)
                               (mapv (fn [[[start op] [end _]]]
                                       [start (- end 1) op]))))]
    (->> operators-pos
         (reduce
          (fn [acc [start end op]]
            (conj acc [op (reduce
                           (fn [a line]
                             (let [cc (reduce
                                       (fn [b [c-index c]]
                                         (update b c-index conj (str c)))
                                       a
                                       (map-indexed vector (subs line start end)))]
                               cc))
                           {}
                           (reverse lines))]))
          []))))

(defn part-two
  [data]
  (let [cols (parser-p2 data)]
    (->> cols
         (reduce
          (fn [acc [op numbers]]
            (let [n (reduce
                     (fn [s [_i nums]]
                       (conj s (Integer/parseInt (str/trim (str/join "" (reverse nums))))))
                     []
                     numbers)]
              (+ acc (eval (concat [(read-string op)] n)))))
          0))))

(deftest test-2025-06
  (testing "part one"
    (is (= 4277556 (part-one input-example)))
    (is (= 4309240495780 (part-one input))))

  (testing "part two"
    (is (= 3263827 (part-two input-example)))
    (is (= 9170286552289 (part-two input)))))

