(ns aoc.2024.15
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [pp-grid.api :as pp]
            [medley.core :as m]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [pathfinding :as pf]))

;; # Parser
(defn parser-p1 [data]
  (let [[raw-grid raw-moves] (str/split data #"\n\n")]
    [(->> (u/to-matrix raw-grid)
          (pf/decode-matrix))
     (mapcat vector (str/replace raw-moves #"\n" ""))]))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/15.txt"))))


(def input-example "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>
v<<")

(def input-larger-example "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

;; Logic
(defn draw
  [cells]
  (->> cells
       (reduce (fn [acc [[y x] c]]
                 (assoc acc [x y] (str c)))
               (pp/empty-grid))))

(def offsets {\< [0 -1]
              \> [0 1]
              \^ [-1 0]
              \v [1 0]})

(defn next-offset-pos [pos offset]
  (mapv + pos offset))

(defn push [next-pos offset cells]
  (when-let [cells-to-move (seq (take-while (fn [coords]
                                              (= (cells coords) \O))
                                            (iterate
                                             #(next-offset-pos % offset)
                                             next-pos)))]
    (let [pos-cell-after (next-offset-pos (last cells-to-move) offset)
          value-cell-after (cells pos-cell-after)]
      (when (= value-cell-after \.)
        [next-pos (-> (reduce
                       (fn [cells coords]
                         (assoc cells (next-offset-pos coords offset) \O))
                       cells
                       cells-to-move)
                      (assoc next-pos \.))]))))

(defn solve-p1 [[{:keys [cells]} moves]]
  (let [start-pos (ffirst (m/filter-vals (fn [v] (= v \@)) cells))
        final-cells (loop [pos start-pos
                           [move & moves] moves
                           cells (assoc cells start-pos \.)]
                      (if-let [offset (offsets move)]
                        (let [next-pos (next-offset-pos pos offset)
                              next-value (cells next-pos)]
                          (case next-value
                            \# (recur
                                pos
                                moves
                                cells)
                            \. (recur
                                next-pos
                                moves
                                cells)
                            \O (if-let [p (push next-pos offset cells)]
                                 (recur
                                  (first p)
                                  moves
                                  (second p))
                                 (recur
                                  pos
                                  moves
                                  cells))))

                        cells))]
    (->> final-cells
         (m/filter-vals (fn [v] (= v \O)))
         (map (fn [[[y x]]]
                (+ (* 100 y) x)))
         (reduce +))))

;; ## Part 1
(defn part-1
  [data]
  (solve-p1 (parser-p1 data)))

;; ## Part 2
(defn parser-p2 [data]
  (let [[raw-grid raw-moves] (str/split data #"\n\n")]
    [(->> (u/to-matrix  (-> raw-grid
                            (str/replace #"\." "..")
                            (str/replace #"@" "@.")
                            (str/replace #"O" "[]")
                            (str/replace #"#" "##")))

          (pf/decode-matrix #(str %)))
     (mapcat vector (str/replace raw-moves #"\n" ""))]))

(def input-p2-example "#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^")

(defn part-2
  [data]
  nil)

(defn get-next-cell [cells [pos value] offset]
  (let [next-pos (next-offset-pos pos offset)
        next-value (cells next-pos)]
    [next-pos next-value]))

(defn horizontal? [move]
  (case move
    (\< \>) true
    false))

(defn ->pos [[pos]]
  pos)

(defn ->value[[_ value]]
  value)

(defn horizontal-move [cells current-cell next-cell]
  (let [[current-pos _current-value] current-cell
        path (take-while
              (fn [[_pos value]]
                (and (not= value ".")
                     (not= value "#")))
              (iterate next-cell current-cell))
        last-path-cell (last path)
        [_ next-path-value] (next-cell last-path-cell)]
    (if (= next-path-value ".")
      (reduce
       (fn [new-cells cell]
         (assoc new-cells (->pos (next-cell cell)) (->value cell)))
       (assoc cells current-pos ".")
       path)
      cells)))

(defn get-current-cell [cells]
   (first (m/filter-vals (fn [v] (= v "@")) cells)))

(defn vertical-move [cells current-cell next-cell]
  (let [[current-pos _current-value] current-cell
        path (take-while
              (fn [[_pos value]]
                (and (not= value ".")
                     (not= value "#")))
              (iterate next-cell current-cell))
        last-path-cell (last path)
        [_ next-path-value] (next-cell last-path-cell)]
    (if (= next-path-value ".")
      (reduce
       (fn [new-cells cell]
         (assoc new-cells (->pos (next-cell cell)) (->value cell)))
       (assoc cells current-pos ".")
       path)
      cells)))

(defn move-robot [cells move]
  (let [current-cell (get-current-cell cells)
        [current-pos _current-value] current-cell
        offset (offsets move)]
    (letfn [(next-cell [cell]
              (get-next-cell cells cell offset))]
      (let [[next-pos next-value] (next-cell current-cell)]
        (cond
          (= next-value ".") (-> cells
                                 (assoc current-pos ".")
                                 (assoc next-pos "@"))
          (horizontal? move) (horizontal-move cells current-cell next-cell)
          :else (vertical-move cells current-cell next-cell))))))

(let [[{:keys [cells]} moves] (parser-p2 input-p2-example)
      _ (println (draw cells))
      r (reduce
         move-robot
         cells
         (take 5 moves))]
  (draw r))

;; # Tests
#_(deftest test-2024-15
    (testing "part one"
      (is (= 1436690 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))



