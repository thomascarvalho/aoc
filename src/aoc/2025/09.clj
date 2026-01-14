(ns aoc.2025.09
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.set :as cset]
            [util :as u]
            [clojure.math.numeric-tower :as tower]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/09.txt"))))

(def input-example "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defn parser [input]
  (->> input
       str/split-lines
       (mapv #(mapv u/parse-int (str/split % #",")))))

(defn part-one
  [data]
  (let [dots (parser data)
        m (->> (for [a dots
                     b dots
                     :when (not= a b)]
                 [(u/manhattan-distance a b) a b]))
        [_ [ay ax] [by bx]] (->> m
                                 (sort-by first >)
                                 first)]
    (* (inc (- (max ay by) (min ay by)))
       (inc (- (max ax bx) (min ax bx))))))


(defn same-y [tiles [y _]]
  (filter #(= y (first %)) tiles))

(defn same-x [tiles [_ x]]
  (filter #(= x (second %)) tiles))

(defn fill-vertical-tiles [tiles]
  (->> (vals (group-by second tiles))
       (reduce
        (fn [acc tiles]
          (let [y (second (first tiles))
                xs (->> (sort-by first tiles)
                        (partition 2 1)
                        (reduce
                         (fn [acc [[x1 _] [x2 _]]]
                           (apply conj acc (range (inc x1) x2)))
                         #{})
                        (mapv (fn [x]
                                [x y])))]
            (apply conj acc xs)))
        #{})))

(defn fill-horizontal-tiles [tiles]
  (->> (vals (group-by first tiles))
       (reduce
        (fn [acc tiles]
          (let [x (ffirst tiles)
                ys (->> (sort-by second tiles)
                        (partition 2 1)
                        (reduce
                         (fn [acc [[_ y1] [_ y2]]]
                           (apply conj acc (range (inc y1) y2)))
                         #{})
                        (mapv (fn [y]
                                [x y])))]
            (apply conj acc ys)))
        #{})))

(defn fill [tiles]
  (->> tiles 
      ((juxt fill-vertical-tiles fill-horizontal-tiles))
      (apply cset/union)))

(defn part-two
  [data]
  (let [red-tiles (set (parser data))
        green-tiles (fill red-tiles)
        all-green-tiles (-> (fill green-tiles)
                            (cset/union green-tiles)
                            (cset/difference red-tiles))]
    (->> all-green-tiles)
    

    ))






#_(deftest test-2025-09
    (testing "part one"
      (is (= 50 (part-one input-example)))
      (is (= 4790063600 (part-one input))))

    (testing "part two"
      (is (= 40 (part-two input-example)))
      (is (= 1393669447690 (part-two input)))))



(time (part-two input-example))

