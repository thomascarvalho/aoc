^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2020.11
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [pp-grid.api :as g]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(def directions [[0 -1]
                 [0 1]
                 [-1 0]
                 [1 0]
                 [1 -1]
                 [-1 -1]
                 [1 1]
                 [-1 1]])

(defn create-grid [cells]
  (let [assoc-data (fn [grid cells] (reduce (fn [acc [coords c]] (assoc acc coords c)) grid cells))]
    (-> (g/empty-grid)
        (assoc-data (into [] cells))
        (g/box :left-padding 1 :right-padding 1))))

(defn adjacents [[x y] cells]
  (for [[x2 y2] directions
        :let    [a (get cells [(+ x x2) (+ y y2)])]
        :when   a]
    a))

(defn adjacents-occupied [coords cells]
  (->> (adjacents coords cells)
       (filter #(= "#" %))))

(defn parser [data]
  (->> data
       str/split-lines
       (map-indexed
        (fn [y row]
          (map-indexed
           (fn [x c]
             {[x y] c}) (str/split row #""))))
       (apply concat)
       (apply merge)))

(def input (->> (slurp (io/resource "inputs/2020/11.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))

;; ## Part 1
(defn part-1
  [initial-cells]

  (->>
   (loop [cells          initial-cells
          previous-cells []]
     (let [new-cells
           (->>
            cells
            (into [])
            (map (fn [[coords c]]
                   (let [occupied (adjacents-occupied coords cells)]
                     (cond
                       (and (= c "L") (empty? occupied)) [coords "#"]
                       (and (= c "#") (>= (count occupied) 4)) [coords "L"]
                       :else [coords c]))))
            (into {}))]
       (if   (= new-cells previous-cells)
         new-cells
         (recur new-cells cells))))
   vals
   (filter #(= "#" %))
   count))
  ;
  


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn adjacents-scale-occupied-count [[initial-x initial-y] cells]
  (->>
   (for [[x2 y2] directions]
     (->>
      (loop [[x y]            [(+ initial-x x2) (+ initial-y y2)]
             directions-cells []]
        (let [a (get cells [x y])]
          (if a
            (recur [(+ x x2) (+ y y2)] (conj directions-cells a))
            directions-cells)))
      (filter #(not= "." %))
      first))
   (remove nil?)
   (remove #(= % "L"))
   count))

(defn part-2
  [initial-cells]
  (->>
   (loop [cells          initial-cells
          previous-cells []]
     (let [new-cells
           (->>
            cells
            (into [])
            (map (fn [[coords c]]
                   (let [occupied-count (adjacents-scale-occupied-count coords cells)]
                     (cond
                       (and (= c "L") (zero? occupied-count)) [coords "#"]
                       (and (= c "#") (>= occupied-count 5)) [coords "L"]
                       :else [coords c]))))
            (into {}))]
       (if (= new-cells previous-cells)
         new-cells
         (recur new-cells cells))))
   #_(create-grid)
   vals
   (filter #(= "#" %))
   count))
  ;
  



;; Tests
(deftest test-2020-11
  #_(testing "part one"
      (is (= 2344 (part-1 input))))

  (testing "adjacents scale"
    (is (= 0 (adjacents-scale-occupied-count [3 3] (parser ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##."))))

    (is (= 8 (adjacents-scale-occupied-count [3 4] (parser ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....")))))

  (testing "part two - example"
      (is (= 26 (part-2 input-example))))
  
  #_(testing "part two"
     (is (= 2076 (part-2 input)))))
  
