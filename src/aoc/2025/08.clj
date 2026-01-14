(ns aoc.2025.08
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [util :as u]
            [clojure.set :as cset]
            [clojure.math.numeric-tower :as tower]
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

(defn- sqr
  "Uses the numeric tower expt to square a number"
  [x]
  (tower/expt x 2))

(defn euclidean-squared-distance
  "Computes the Euclidean squared distance between two sequences"
  [a b]
  (reduce + (map (comp sqr -) a b)))

(defn euclidean-distance
  "Computes the Euclidean distance between two sequences"
  [a b]
  (tower/sqrt (euclidean-squared-distance a b)))

(defn euclidean-distance [v1 v2]
  (->> (map - v1 v2)
       (map #(* % %))
       (reduce +)
       (tower/sqrt)))


(defn get-circuits [circuits a b]
  (->> circuits
       (map-indexed vector)
       (reduce
        (fn [m [i circuit]]
          (cond
            (circuit a) (assoc m :circuit-a-index i)
            (circuit b) (assoc m :circuit-b-index i)
            :else m))
        {:circuit-a-index nil
         :circuit-b-index nil})))

(defn part-one
  [data]
  (let [lines (parser data)
        all (for [a  lines
                  b lines
                  :when (not= a b)]
              [(euclidean-distance a b) (set [a b])])
        sorted (sort-by first (set all))]
    (->> sorted
        (take 9)
        (reduce
         (fn [acc [_ s]]
           (let [inter (filter (fn [[i v]] (seq (cset/intersection v s))) (map-indexed vector acc))]
             (case (count inter)
               0 (conj acc s)
               1 (let [[i v] (first inter)]
                   (update acc i cset/union s))
               2 acc)))
         [])
        #_#_#_#_(map count)
        (sort >)
        (take 3)
        (apply *))))



(defn part-two
  [data]
  (let [lines (parser data)]))


#_(deftest test-2025-08
    (testing "part one"
      (is (= 21 (part-one input-example)))
      (is (= 1516 (part-one input))))

    (testing "part two"
      (is (= 40 (part-two input-example)))
      (is (= 1393669447690 (part-two input)))))



;; 2028 too low

(time (part-one input-example))

