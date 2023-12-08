^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.03
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [instaparse.core :as insta]
            [test-util :as t]
            [util :as u :refer [parse-int instaparse]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "03" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(def directions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1]  [1 -1] [1 0] [1 1]])

(defn neighbours-symbols [[x y] cells]
  (for [[x2 y2] directions
        :let    [new-x (+ x x2)
                 new-y (+ y y2)
                 v (filter (fn [[coords t]]
                             (and
                              (= coords [new-x new-y])
                              (= t :S))) cells)]
        :when   (seq v)]
    v))

(defn neighbours-digits [[x y] cells]
  (->>
   (for [[x2 y2] directions
         :let    [new-x (+ x x2)
                  new-y (+ y y2)
                  digits (->> cells
                              (filter (fn [[coords t]]
                                        (and
                                         (= coords [new-x new-y])
                                         (= t :D)))))]
         :when   (seq digits)]
     (->> digits
          (map
           (fn [[_ _ v index]]
             [index (parse-int v)])))) ;; unique index + value
   (apply concat)
   (group-by first) ;; group by unique index
   (map #(-> % second first second)))) ;; and get values


(defn parser [data]
  (->> data
       str/split-lines))

(def input (->> (slurp (io/resource "inputs/2023/03.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example #_(parser "467..467..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")
  (parser "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."))

(defn spans [t]
  (if (sequential? t)
    (cons (insta/span t) (map spans (next t)))
    t))

;; ## Part 1 
(defn part-1
  [lines]
  (let [cells (->>
               lines
               (map-indexed (fn [y s]
                              (instaparse s "R = (S | <E> | D)*
                       D = #'\\d+'
                       E = '.'
                       S = #'[^.\\d]'
                       " {:R (fn [& cells]
                               (->> (for [[t [min-x max-x] v] (map #(conj (spans %) (first %)) cells)
                                          x                   (range min-x max-x)]
                                      [[x y] t v [y min-x max-x]])
                                    (group-by #(nth % 3))
                                    (map second)
                                    (apply concat)))})))
               (apply concat))]
    (->>
     (for [[current-coords _ v index] (filter (fn [[_ t]]
                                                (= t :D)) cells)
           :when                      (seq (neighbours-symbols current-coords cells))]
       [index (parse-int v)])
     (into #{})
     (map second)
     (reduce + 0)))
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [lines]
  (let [cells (->>
               lines
               (map-indexed (fn [y s]
                              (instaparse s "R = (S | <E> | D)*
                       D = #'\\d+'
                       E = '.'
                       S = #'[^.\\d]'
                       " {:R (fn [& cells]
                               (for [[t [min-x max-x] v] (map #(conj (spans %) (first %)) cells)
                                     x                   (range min-x max-x)]
                                 [[x y] t v [y min-x max-x]]) ;; coords type value index
                               )})))
               (apply concat))]
    (->>
     (for [[current-coords] (filter (fn [[_ _ v]]
                                      (= v "*")) cells)
           :let             [n (neighbours-digits current-coords cells)]
           :when            (>= (count n) 2)]
       (reduce * n))
     (reduce + 0)))
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-03
  (testing "some tests"
    (is (= 24 (part-1 (parser "........
.24.24..
......*."))))

    (is (= 27 (part-1 (parser "23.4
..*."))))

    (is (= 925 (part-1 (parser "12.......*..
+.........34
.......-12..
..78........
..*....60...
78.........9
.5.....23..$
8...90*12...
............
2.2......12.
.*.........*
1.1..503+.56"))))

    (is (= 40 (part-1 (parser ".......5......
..7*..*.......
...*13*.......
.......15.....")))))

  (testing "part one - example"
      (is (= 4361 (part-1 input-example))))

  (testing "part one"
      (is (= 533784 (part-1 input))))

  (testing "part two"
    (is (= 78826761 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
#_(t/render-results (t/run #'test-2023-03))