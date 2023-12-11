^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.10
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "10" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(def pipe-dirs {\| [:N :S]
                \- [:W :E]
                \7 [:W :S]
                \J [:W :N]
                \L [:E :N]
                \F [:E :S]
                \S [:W :E]})

(defn parser [data]
  (->> data
       u/to-matrix))

(def input (->> (slurp (io/resource "inputs/2023/10.txt"))
                parser))
{:nextjournal.clerk/visibility {:result :hide}}

;;  Exa;lmbnn;;nvllllllmmple
(def input-example (parser "..F7.
.FJ|.
SJ.L7
|F--J
LJ..."))

(def directions  [[:E [1 0]]
                  [:S [0 1]]
                  [:W [-1 0]]
                  [:N [0 -1]]])

(defn neighbours [[[x y] _c] m]
  (for [[dir [x2 y2]] directions
        :let          [n-coords [(+ x x2) (+ y y2)]
                       target (get m n-coords)]
        :when         (and target (not= target \.))]
    [n-coords target]))

(defn matrix->map [m]
  (->> m
       (map-indexed
        (fn [y row]
          (->> row
               (map-indexed
                (fn [x c]
                  (hash-map [x y] c)))
               (into {}))))
       (into {})))

(defn find-possible-neighbours [[[x y] c]]
  (case c
    \| (concat
        (map (fn [t]
               [[x (dec y)] t]) [\| \7 \F])
        (map (fn [t]
               [[x (inc y)] t]) [\| \J \L]))
    (\S \-) (concat
             (map (fn [t]
                    [[(dec x) y] t]) [\- \L \F \S])
             (map (fn [t]
                    [[(inc x) y] t]) [\- \7 \J \S]))
    \7 (concat
        (map (fn [t]
               [[(dec x) y] t]) [\- \L \F])
        (map (fn [t]
               [[x (inc y)] t]) [\| \L \J]))
    \J (concat
        (map (fn [t]
               [[(dec x) y] t]) [\- \L \F])
        (map (fn [t]
               [[x (dec y)] t]) [\| \F \7]))
    \L (concat
        (map (fn [t]
               [[(inc x) y] t]) [\- \J \7])
        (map (fn [t]
               [[x (dec y)] t]) [\| \F \7]))
    \F (concat
        (map (fn [t]
               [[(inc x) y] t]) [\- \7 \J])
        (map (fn [t]
               [[x (inc y)] t]) [\| \L \J]))))


(defn choose [[[x y] c] neighbours path]
  (->
   (for [[[x2 y2] c2] neighbours
         :let         [n (some #{[[x2 y2] c2]} (find-possible-neighbours [[x y] c]))]
         :when        (and n (not (some #{n} path)))]

     n)
   first))

;; ## Part 1
(defn part-1
  [matrix]
  (let [m     (matrix->map matrix)
        start (first (filter #(= (second %) \S) m))
        path  (loop [current start
                     path    []
                     step    0]
                (if (or (= start (last path)) (= step 1000000000))
                  path

                  (let [n         (neighbours current m)
                        next-cell (choose current n path)]

                    (recur next-cell (conj path next-cell) (inc step)))))]
    (/ (count path) 2)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]
  nil
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :hide}}
#_(part-2 input)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}
 :clerk/no-cache               true}
(deftest test-2023-10
  (testing "part one"
    (is (= 6682 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}



