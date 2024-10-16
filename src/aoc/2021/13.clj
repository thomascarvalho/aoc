^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.13
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [pp-grid.api :as g]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "13" "2021"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (let [[raw-coords raw-folds] (str/split data #"\n\n")
        coords                 (->> raw-coords
                                    str/split-lines
                                    (mapv #(->>
                                            (str/split % #",")
                                            (mapv parse-int)))
                                    sort)
        folds                  (->> raw-folds
                                    str/split-lines
                                    (map #(re-find #"(\w)=(\d+)" %))
                                    (mapv (fn [[_ x-or-y number]]
                                            [(keyword x-or-y)  (parse-int number)])))]


    [coords folds]))

(def input (->> (slurp (io/resource "inputs/2021/13.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"))

(defn split [coords [axis number]]
  (let [axis-filter (case axis
                      :y second
                      :x first)]

    [(filter #(< (axis-filter %) number) coords)
     (filter #(> (axis-filter %) number) coords)]))

(defn create-grid [coords]
  (let [assoc-data (fn [grid items c] (reduce (fn [acc k] (assoc acc k c)) grid items))]
    (-> (g/empty-grid)
        (assoc-data coords "#")
        (g/box :left-padding 1 :right-padding 1))))

(defn get-new-coords [coords-to-migrate [axis n]]
  (->> coords-to-migrate
       (map (fn [[x y]]
              (case axis
                :y (let [new-y (- n (- y n))]
                     [x new-y])
                :x (let [new-x (- n (- x n))]
                     [new-x y]))))))

(defn fold [coords instruction]
  (let [[new-shape-coords coords-to-migrate] (split coords instruction)
        added-coords                         (get-new-coords coords-to-migrate instruction)]
    (set (concat new-shape-coords added-coords))))

;; ## Part 1
(defn part-1
  [[initial-coords initial-instructions]]
  (->
   (fold initial-coords (first initial-instructions))
   count))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [[initial-coords initial-instructions]]
  (->
   (loop [coords       initial-coords
          instructions initial-instructions]
     (if (seq instructions)
       (recur (fold coords (first instructions)) (next instructions))
       coords))
   create-grid))

  ;
  

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; Tests
(deftest test-2021-13
  (testing "part one"
    (is (= 802 (part-1 input))))

  (testing "part two"
    (is (= (part-2 input)
           "+-----------------------------------------+
| ###  #  # #  # #### ####  ##  #  # ###  |
| #  # # #  #  # #       # #  # #  # #  # |
| #  # ##   #### ###    #  #    #  # ###  |
| ###  # #  #  # #     #   # ## #  # #  # |
| # #  # #  #  # #    #    #  # #  # #  # |
| #  # #  # #  # #    ####  ###  ##  ###  |
+-----------------------------------------+"))))

