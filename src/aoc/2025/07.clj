(ns aoc.2025.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [util :as u]
            [pathfinding :as pf]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/07.txt"))))

(def input-example ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(defn parser-p1 [input]
  (->> input
       str/split-lines
       pf/decode-matrix))


(defn part-one
  [data]
  (let [{:keys [height cells]} (parser-p1 data)
        beams-coords (set (map first (filter (fn [[_ v]] (= v \^)) cells)))
        start (first (map first (filter (fn [[_ v]] (= v \S)) cells)))]
    (loop [y 0
           tachyons #{(second start)}
           splits 0]
      (if (>= y height)
        splits
        (let [x-to-splits (map second (filter (fn [[by bx]] (and (= by y) (tachyons bx))) beams-coords))]
          (if (seq x-to-splits)
            (let [new-tachyons (reduce
                                (fn [t x]
                                  (as-> t $
                                    (remove #{x} $)
                                    (set $)
                                    (conj $ (inc x) (dec x))))
                                tachyons
                                x-to-splits)]
              (recur
               (inc y)
               new-tachyons
               (+ splits (count x-to-splits))))
            (recur
             (inc y)
             tachyons
             splits)))))))

(defn part-two
  [data]
  (let [{:keys [height cells]} (parser-p1 data)
        beams-coords (set (map first (filter (fn [[_ v]] (= v \^)) cells)))
        start (first (map first (filter (fn [[_ v]] (= v \S)) cells)))]
    (loop [paths {start 1}]
      (if (>= (first (ffirst paths)) height)
        (apply + (vals paths))
        (let [new-paths (->> paths
                             (reduce
                              (fn [acc current-path]
                                (let [[[y x] n]  current-path]
                                  (if (beams-coords [(inc y) x])
                                    (let [a1 [(inc y) (dec x)]
                                          a2 [(inc y) (inc x)]]
                                      (-> acc
                                          (update a1 (fnil + 0) n)
                                          (update a2 (fnil + 0) n)))
                                    (let [a [(inc y) x]]
                                      (update acc a (fnil + 0) n)))))
                              {}))]
          (recur
           new-paths))))))

(deftest test-2025-07
  (testing "part one"
    (is (= 21 (part-one input-example)))
    (is (= 1516 (part-one input))))

  (testing "part two"
    (is (= 40 (part-two input-example)))
    (is (= 1393669447690 (part-two input)))))

