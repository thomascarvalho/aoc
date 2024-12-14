^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.15
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [pathfinding :as pf]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "15" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (pf/decode-matrix)))

(def input (->> (slurp (io/resource "inputs/2018/15.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######"))


(def input-moves-example (parser "#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########"))

(defn- neighbors [cell grid]
  (let [[y x] cell]
    (filter #(when-let [v (grid %)]
               (not= v \#))
            [[(dec y) x] [y (inc x)] [(inc y) x] [y (dec x)]])))

(defn- get-units [cells]
  (->> cells
       (filter (fn [[_ v]] (some #(= % v) [\E \G])))
       (sort-by (juxt ffirst (comp second first)))))

(let [{:keys [cells]} input-moves-example
      units (get-units cells)]
  (loop [step 0
         cells cells]
    (let [units (get-units cells)]
      (if (= step 1)
        (do #_#_(println "---")
            (println (pf/draw-grid cells))
            nil)
        (let [new-cells (reduce (fn [cells [coords cell-type]]
                                  (if (or (= cell-type \#) (= cell-type \.))
                                    cells
                                    (let [opponents (filter (fn [[_ opponent-race]]
                                                              (= opponent-race (if (= cell-type \E)
                                                                                 \G
                                                                                 \E))) (get-units cells))
                                          shortest-path (reduce (fn [path [opponent-coords]]
                                                                  (let [path-to-opponent (pf/bfs cells coords opponent-coords neighbors)]
                                                                    (if (or (nil? path) (< (count path) (count path-to-opponent)))
                                                                      path-to-opponent
                                                                      path)))
                                                                nil opponents)]
                                      
                                      (if (= (count shortest-path) 1)
                                        cells
                                        (-> cells
                                            (assoc (second shortest-path) cell-type)
                                            (assoc coords \.))))))
                                cells cells)]
          (recur (inc step)
                 new-cells)))))


  #_(pf/bfs cells [2 4] [4 3] neighbors))

;; ## Part 1
(defn part-1
  [data]
  data)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
#_(deftest test-2018-15
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/test-render #'test-2018-15)
