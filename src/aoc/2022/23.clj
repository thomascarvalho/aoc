^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.23
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [pp-grid.api :as g]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "23" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn create-grid [elves]
  (let [assoc-data (fn [grid items c] (reduce (fn [acc k] (assoc acc k c)) grid items))]
    (-> (g/empty-grid)
        (assoc-data elves "#")
        #_(assoc-data beacons "B")
        #_(assoc-data no-beacons "#")
        (g/box :left-padding 1 :right-padding 1))))


(defn parser [data]
  (->> data
       str/split-lines
       (map-indexed
        (fn [y row]
          (map-indexed
           (fn [x cell]
             (when (= cell \#) [x y])) (char-array row))))
       (apply concat)
       (remove nil?)))

(def input (->> (slurp (io/resource "inputs/2022/23.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "..............
..............
.......#......
.....###.#....
...#...#.#....
....#...##....
...#.###......
...##.#.##....
....#..#......
..............
..............
.............."))

(def directions-mapping {:N  [0 -1]
                         :S  [0 1]
                         :W  [-1 0]
                         :E  [1 0]
                         :NE [1 -1]
                         :NW [-1 -1]
                         :SE [1 1]
                         :SW [-1 1]})


(defn neighbours [[x y] directions]
  (map (fn [dir]
         (let [[x2 y2] (directions-mapping dir)]
           [(+ x x2) (+ y y2)])) directions))

(def all-directions (into [] (keys directions-mapping)))

(defn all-free-space? [neighbours-coords elves]
  (nil? (some (set neighbours-coords) elves)))

(defn propose-elf-move [elf elves directions-priorities]
  (if (all-free-space? (neighbours elf all-directions) elves)
    elf
    (loop [all-dirs directions-priorities]
      (if (seq all-dirs)
        (let [dirs (first all-dirs)]
          (if (all-free-space? (neighbours elf dirs) elves)
            (first (neighbours elf [(first dirs)]))
            (recur (next all-dirs))))
        elf))))

(defn get-all-elves-moves [elves directions-priorities]
  (map #(propose-elf-move % elves directions-priorities) elves))

(defn move-directions-priorities [dirs]
  (concat (next dirs) (take 1 dirs)))

;; ## Part 1
(defn part-1
  [data]
  (let [initial-directions-priorities [[:N :NE :NW] [:S :SE :SW] [:W :NW :SW] [:E :NE :SE]]
        initial-elves                 (into [] data)
        final-state                   (loop [elves                 initial-elves
                                             directions-priorities initial-directions-priorities
                                             rounds                0]
                                        (if (= rounds 10)
                                          elves
                                          (let [proposals       (get-all-elves-moves elves directions-priorities)
                                                proposals-freqs (frequencies proposals)
                                                new-elves       (for [[i elf] (map-indexed (fn [i e] [i e]) elves)
                                                                      :let    [elf-propose (nth proposals i)]]

                                                                  (if (> (proposals-freqs elf-propose) 1)
                                                                    elf
                                                                    elf-propose))]
                                            (recur new-elves (move-directions-priorities directions-priorities) (inc rounds)))))
        all-x                         (map first final-state)
        all-y                         (map second final-state)
        min-x                         (apply min all-x)
        max-x                         (apply max all-x)
        min-y                         (apply min all-y)
        max-y                         (apply max all-y)]
    (- (* (inc (- max-x min-x)) (inc (- max-y min-y))) (count final-state))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [initial-directions-priorities [[:N :NE :NW] [:S :SE :SW] [:W :NW :SW] [:E :NE :SE]]
        initial-elves                 (into [] data)]
    (loop [elves                 initial-elves
           directions-priorities initial-directions-priorities
           previous-elves        []
           rounds                0]
      (if (= previous-elves elves)
        rounds
        (let [proposals       (get-all-elves-moves elves directions-priorities)
              proposals-freqs (frequencies proposals)
              new-elves       (for [[i elf] (map-indexed (fn [i e] [i e]) elves)
                                    :let    [elf-propose (nth proposals i)]]

                                (if (> (proposals-freqs elf-propose) 1)
                                  elf
                                  elf-propose))]
          (recur new-elves (move-directions-priorities directions-priorities) elves (inc rounds)))))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2022-23
  (testing "directions mapping coords"
    (is (= [[0 -1]] (neighbours [0 0] [:N])))
    (is (= [[0 -1] [1 -1] [-1 -1]] (neighbours [0 0] [:N :NE :NW])))
    (is (= [[0 1] [-1 1]] (neighbours [0 0] [:S :SW]))))
  (testing "part one"
    (is (= 3684 (part-1 input))))

  #_(testing "part two"
    (is (= 862 (part-2 input)))))