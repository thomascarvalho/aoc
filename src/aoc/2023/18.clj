^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.18
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [pp-grid.api :as g]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
(def dirs {:U [-1 0]
           :R [0 1]
           :D [1 0]
           :L [0 -1]})

(defn create-grid [dig-plan]
  (let [assoc-data (fn [grid items c] (reduce (fn [acc k] (assoc acc (vec (reverse k)) c)) grid items))]
    (-> (g/empty-grid)
        (assoc-data dig-plan "#"))))

(defn parser [data]
  (->> data
       u/to-lines
       (map (fn [s]
              (let [[dir step color] (str/split s #" ")]
                {:dir   (keyword dir)
                 :step  (parse-long step)
                 :color (subs color 1 (dec (count color)))})))))

(def input (->> (slurp (io/resource "inputs/2023/18.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"))

(defn move [[y x] {:keys [dir step]}]
  (mapv + [y x] (mapv * [step step] (dir dirs))))

;; Magic `shoelace` formula https://en.wikipedia.org/wiki/Shoelace_formula
(defn shoelace [[[y1 x1] [y2 x2]]]
  (- (* x1 y2) (* y1 x2)))

;; Apply `shoelace` over adjacent pairs 
(defn calculate-full-area [[polygon points]]
  (let [poly (reverse polygon)]
    (->
     (+ (shoelace (list (last poly) (first poly)))
        (reduce + (map shoelace (partition 2 1 poly)))
        points)
     (quot 2)
     inc)))

(defn create-polygon [actions]
  (loop [[action & next-actions] actions
         polygon                 [[0 0]]
         points                  0]
    (if action
      (let [{:keys [step]} action
            newpos         (move (first polygon) action)]
        (recur next-actions (cons newpos polygon) (+ points step)))
      [polygon points])))

;; ## Part 1
(defn part-1
  [actions]
  (->> actions
       create-polygon
       calculate-full-area))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn decode-hexa-color [color]
  {:dir  (case (last color)
           \0 :R
           \1 :D
           \2 :L
           \3 :U)
   :step (Integer/parseInt (subs color 1 (dec (count color))) 16)})

(defn part-2
  [actions]
  (->> actions
       (map #(-> % :color decode-hexa-color))
       create-polygon
       calculate-full-area))



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-18

  (testing "part one - example"
    (is (= 62 (part-1 input-example))))

  (testing "part one"
    (is (= 34329 (part-1 input))))

  (testing "decode hexa color"
    (is (= {:dir  :R
            :step 461937} (decode-hexa-color "#70c710")))

    (is (= {:dir  :D
            :step 56407} (decode-hexa-color "#0dc571"))))

  (testing "part two"
    (is (= 42617947302920 (part-2 input)))))
