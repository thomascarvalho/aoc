^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.14
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.string :as cs]
            [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [instaparse.core :as insta]
            [clojure.core.matrix :as m]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "15" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

(def input (->> (slurp (io/resource "inputs/2022/14.txt"))))

(def input-example "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(defn parser [d]
  (->> d
       ((insta/parser
         " <S> = COORDS*
            COORDS = X <','> Y <' -> '>?
            INT = #'-?\\d+'
            X = INT
            Y = INT

"))
       #_{:clj-kondo/ignore [:unresolved-var]}
       (insta/transform {:INT parse-int
                         :S concat
                         :COORDS (fn [[_ x] [_ y]]
                                   [x y])})))
;; LOGICS

(defn get-range [start end]
  (if (< start end)
    (range (dec start) end)
    (range (dec end) start)))

(defn get-limits [coords]
  (let [all-x (map first coords)
        all-y (map second coords)
        min-x (reduce min all-x)
        max-x (reduce max all-x)
        max-y (reduce max all-y)]
    [min-x (inc max-x) (inc max-y)]))

(defn paths->coords [paths]
  (->>
   (for [path paths
         :let [all-coords (partition 2 1 path)]]
     (mapcat
      (fn [[[start-1 end-1] [start-2 end-2]]]
        (if (= start-1 start-2)
          (map (fn [y] [start-1 (inc y)])
               (get-range end-1 end-2))
          (map (fn [x] [(inc x) end-1])
               (get-range start-1 start-2))))
      all-coords))
   (mapcat concat)
   set))

;; PRINTS


(defn get-cell [grid x y]
  (try
    (m/mget grid y x)
    (catch Exception e nil)))

(defn get-cell-p2 [grid x y infinite-y]
  (if (= y infinite-y)
    "#"
    (try
      (m/mget grid y x)
      (catch Exception e nil))))

(def blocked? #(or (= % "#") (= % "o")))

(def is-empty? #(= % "."))

(defn fall [grid [current-x current-y]]
  (let [new-y (->> current-x
                   (m/select grid :all)
                   (map-indexed (fn [idx itm] [idx itm]))
                   (filter (fn [[idx v]] (and (blocked? v) (> idx current-y))))
                   first
                   first)]
    (when new-y
      [current-x (dec new-y)])))

(defn fall-p2 [grid [current-x current-y] infinite-y]
  (let [new-y (->> current-x
                   (m/select grid :all)
                   (map-indexed (fn [idx itm] [idx itm]))
                   (filter (fn [[idx v]] (and (blocked? v) (> idx current-y))))
                   first
                   first)]

    (if  new-y
      [current-x (dec new-y)]
      [current-x (dec infinite-y)])))

(defn get-next-falling-position [grid [current-x current-y]]
  (when current-x
    (cond
      (is-empty? (get-cell grid (dec current-x) (inc current-y))) [(dec current-x) (inc current-y)]  ;; diagonale en bas a gauche
      (is-empty? (get-cell grid (inc current-x) (inc current-y))) [(inc current-x) (inc current-y)] ;; diagonale en bas a droite
      :else nil)))

(defn get-next-falling-position-p2 [grid [current-x current-y] infinite-y]
  (when current-x
    (cond
      (is-empty? (get-cell-p2 grid (dec current-x) (inc current-y) infinite-y)) [(dec current-x) (inc current-y)]  ;; diagonale en bas a gauche
      (is-empty? (get-cell-p2 grid (inc current-x) (inc current-y) infinite-y)) [(inc current-x) (inc current-y)] ;; diagonale en bas a droite
      :else nil)))



;; CORE & TESTS

(defn part-one []
  (let [blocks-coords (->> (str input)
                           cs/split-lines
                           (map parser)
                           paths->coords)
        [min-x max-x max-y] (get-limits blocks-coords)
        grid (m/mutable (apply vector
                               (for [y (range 0 max-y)]
                                 (apply vector (for [x (range 0 max-x)]
                                                 ".")))))]


    (doseq [[x y] blocks-coords]
      (m/mset! grid y x "#"))

    (loop [units 0]
      (let [target-pos
            (loop [pos [500 0]]
              (let [blocked-at-pos (fall grid pos)
                    next-pos (get-next-falling-position grid blocked-at-pos)]
                (if next-pos
                  (recur next-pos)
                  blocked-at-pos)))]
        (if (and target-pos (not= target-pos [500 0]) (< (second target-pos) (m/row-count grid)))
          (do
            (m/mset! grid (second target-pos) (first target-pos) "o")
            (recur
             (inc units)))
          units)))))

(defn part-two []
  (let [data input]
    0))

#_(deftest test-2022-14
    #_(testing " part one "
       (is (= 1 (part-one))))

    #_(testing " part two "
       (is (= 1 (part-two)))))



