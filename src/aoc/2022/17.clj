^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.17
  {:nextjournal.clerk/toc true}
  (:require [util :as u]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as cset]
            [nextjournal.clerk :as clerk]
            [pp-grid.api :as g]
            [clojure.test :refer :all]))

; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "17" "2022"))

; # Solution

{:nextjournal.clerk/visibility {:result :hide}}
(def input (-> (slurp (io/resource "inputs/2022/17.txt"))
               (str/replace #"\n" ""))) ;; Load the resource


(def input-example ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

; ## Part 1


{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(def SHAPES [:horizontal-line :cross :triangle :vertical-line :square])

(def ROCKS {:horizontal-line [["#" "#" "#" "#"]]
            :cross           [["." "#" "."]
                              ["#" "#" "#"]
                              ["." "#" "."]]
            :triangle        [["." "." "#"]
                              ["." "." "#"]
                              ["#" "#" "#"]]
            :vertical-line   [["#"]
                              ["#"]
                              ["#"]
                              ["#"]]
            :square          [["#" "#"]
                              ["#" "#"]]})

(defn get-moves [data]
  (->>
   data
   char-array
   (map #(case %
           \> :right
           \< :left))))

(defn get-floor [blocks]
  (let [y (if (seq blocks) (inc (apply max (map second blocks))) 4)]
    (map (fn [x] [x y]) (range 7))))

(defn get-min-y [blocks]
  (if (seq blocks) (- (reduce min (map second blocks)) 4) 0))

(defn create-grid [current-rock blocks floor]
  (let [assoc-data (fn [grid items c] (reduce (fn [acc k] (assoc acc k c)) grid items))]
    (-> (g/empty-grid)
        (assoc-data current-rock "@")
        (assoc-data blocks "#")
        (assoc-data floor "F")
        #_(g/box :left-padding 1 :right-padding 1))))


(defprotocol Rock
  (coords [_ [x y]])
  (position [_ coords]))

(defrecord HorizontalLine []
  Rock
  (coords [_ [x y]] [[x y] [(inc x) y] [(+ x 2) y] [(+ x 3) y]])
  (position [_ c] (first c)))

(defrecord Cross []
  Rock
  (coords [_ [x y]] [[(inc x) (- y 2)]
                     [x (dec y)] [(inc x) (dec y)] [(+ x 2) (dec y)]
                     [(inc x) y]])


  (position [_ c] (let [[x y-dec] (second c)]
                    [x (inc y-dec)])))
(defrecord Triangle []
  Rock
  (coords [_ [x y]] [[(+ x 2) (- y 2)]
                     [(+ x 2) (dec y)]
                     [x y] [(inc x) y] [(+ x 2) y]])
  (position [_ c] (nth c 2)))

(defrecord VerticalLine []
  Rock
  (coords [_ [x y]] [[x (- y 3)]
                     [x (- y 2)]
                     [x (dec y)]
                     [x y]])
  (position [_ c] (nth c 3)))

(defrecord Square []
  Rock
  (coords [_ [x y]] [[x (dec y)] [(inc x) (dec y)]
                     [x y] [(inc x) y]])
  (position [_ c] (nth c 2)))


(defn move [current-move current-rock]
  (case current-move
    :left (map (fn [[x y]] [(dec x) y]) current-rock)
    :right (map (fn [[x y]] [(inc x) y]) current-rock)
    :down (map (fn [[x y]] [x (inc y)]) current-rock)))

(defn valid-pos? [rock blocks y-floor]
  (let [all-x (map first rock)
        min-x (reduce min all-x)
        max-x (reduce max all-x)
        all-y (map second rock)
        max-y (reduce max all-y)]
    (and
     (>= min-x 0)
     (<= max-x 6)
     (< max-y y-floor)
     (empty? (cset/intersection (set rock) blocks)))))

(def repeated-shapes (cycle [(->HorizontalLine) (->Cross) (->Triangle) (->VerticalLine) (->Square)]))

(defn get-tower-size [blocks]
  (let [all-y (map second blocks)]
    (inc (+ (abs (reduce min all-y)) (abs (reduce max all-y))))))

(defn get-pos [pos blocks]
  (let [min-y         (get-min-y blocks)]
    (if (= pos :initial) [2 min-y] pos)))

(defn maybe-move-left-or-right [current-move current-rock blocks y-floor]
  (let [new-rock (move current-move current-rock)]
    (if (valid-pos? new-rock blocks y-floor)
      new-rock
      (when-not (= current-move :down) current-rock))))

(defn maybe-move-down [current-rock blocks y-floor]
  (let [new-rock (move :down current-rock)]
    (when (valid-pos? new-rock blocks y-floor)
      new-rock)))

(defn get-floor-y [blocks]
  (reduce max (map second (get-floor blocks))))

(defn add-rock [rock blocks]
  (apply conj blocks rock))

(defn part-1 []
  (loop [shapes   repeated-shapes
         moves    (cycle (get-moves input-example))
         blocks   #{}
         pos      :initial
         nb-rocks 0]
    (let [current-shape (first shapes)
          current-move  (first moves)
          current-rock  (coords current-shape (get-pos pos blocks))
          floor-y       (get-floor-y blocks)]

      (if (>= nb-rocks 2022)
        (get-tower-size blocks)
        (let [left-or-right-rock (maybe-move-left-or-right current-move current-rock blocks floor-y)
              down-rock          (maybe-move-down left-or-right-rock blocks floor-y)]
          (if down-rock
            (recur shapes (next moves) blocks (position current-shape down-rock) nb-rocks)
            (recur (next shapes) (next moves) (add-rock left-or-right-rock blocks) :initial (inc nb-rocks)))))

        ;
      )))

(comment

  (part-1)

  ;
  )

; ## Part 2


#_(comment
    (defn part-2 []
      (let [data input]
        0))




    {:nextjournal.clerk/visibility {:code   :hide
                                    :result :hide}}
    (deftest test-2022-17
      (testing "part one"
        (is (= 1 (part-1))))

      (testing "part two"
        (is (= 1 (part-2))))))


