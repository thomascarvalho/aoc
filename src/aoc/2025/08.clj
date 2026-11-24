(ns aoc.2025.08
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [util :as u]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/08.txt"))))

(def input-example "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(defn parser [input]
  (->> input
       str/split-lines
       (mapv #(mapv u/parse-int (str/split % #",")))))

(defn squared-distance [a b]
  (reduce + (map #(* (- %1 %2) (- %1 %2)) a b)))

(defn sorted-pairs
  "Every pair of points once, closest first."
  [points]
  (->> (for [[i a] (map-indexed vector points)
             b (subvec points (inc i))]
         [a b])
       (sort-by (fn [[a b]] (squared-distance a b)))))

;; ## Union-find

(defn uf-init [xs]
  (zipmap xs xs))

(defn uf-find [parent x]
  (let [p (parent x)]
    (if (= p x)
      x
      (recur parent p))))

(defn uf-union [parent a b]
  (let [ra (uf-find parent a)
        rb (uf-find parent b)]
    (if (= ra rb)
      parent
      (assoc parent ra rb))))

(defn part-one
  [data pairs]
  (let [points (parser data)
        parent (reduce (fn [parent [a b]] (uf-union parent a b))
                       (uf-init points)
                       (take pairs (sorted-pairs points)))]
    (->> points
         (map #(uf-find parent %))
         frequencies
         vals
         (sort >)
         (take 3)
         (reduce *))))

(defn part-two
  [data]
  (let [points (parser data)]
    (loop [[[a b] & others] (sorted-pairs points)
           parent (uf-init points)
           n (count points)]
      (let [ra (uf-find parent a)
            rb (uf-find parent b)]
        (cond
          (= ra rb) (recur others parent n)
          (= n 2) (* (first a) (first b))
          :else (recur others (assoc parent ra rb) (dec n)))))))

(deftest test-2025-08
  (testing "part one"
    (is (= 40 (part-one input-example 10)))
    (is (= 83520 (part-one input 1000))))

  (testing "part two"
    (is (= 25272 (part-two input-example)))
    (is (= 1131823407 (part-two input)))))
