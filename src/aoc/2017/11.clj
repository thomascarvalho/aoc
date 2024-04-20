^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.11
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "11" "2017"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (-> data
      (str/replace #"\n" "")
      (str/split #",")))

(def input (->> (slurp (io/resource "inputs/2017/11.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "ne,ne,s,s"))

;; CELL

;; https://www.redblobgames.com/grids/hexagons/
;; q (w->e), s (se->nw) r (ne->sw)

;; s [0 -1 1]
;; sw [-1 0 1]
;; se [1 -1 0]
;; n [0 1 -1]
;; nw [-1 1 0]
;; ne [1 0 -1]

(def dirs {"s"  [0 -1 1]
           "sw" [-1 0 1]
           "se" [1 -1 0]
           "n"  [0 1 -1]
           "nw" [-1 1 0]
           "ne" [1 0 -1]})

(defn hex-distance [a b]
  (->
   (->>
    (mapv - a b)
    (map abs)
    (apply +))
   (/ 2)))

;; ## Part 1
(defn part-1
  [data]
  (let [moves data
        path  (reduce
               (fn [path d]
                 (let [pos (mapv + (last path) (dirs d))]
                   (conj path pos))) [[0 0 0]] moves)]
    (hex-distance (first path) (last path))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [moves          data
        [start & path] (reduce
                        (fn [path d]
                          (let [pos (mapv + (last path) (dirs d))]
                            (conj path pos))) [[0 0 0]] moves)]
    (reduce (fn [m end]
              (max m (hex-distance start end))) 0 path)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-11
  (testing "part one"
    (is (= 664 (part-1 input))))

  (testing "part two"
    (is (= 1447 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(t/test-render #'test-2017-11)