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
(defn draw [robot cells]
  (->> cells
       (reduce (fn [acc [[y x] c]]
                 (assoc acc [x y] (if (= robot [y x]) "@" (str c))))
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

#_(part-1 input)
#_(part-1 input-example)


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

(defn push-p2 [next-value next-pos offset cells]
  (when-let [cells-to-move (seq (take-while (fn [coords]
                                              (or (= (cells coords) \[)
                                                  (= (cells coords) \])))
                                            (iterate
                                             #(next-offset-pos % offset)
                                             next-pos)))]
    (let [pos-cell-after (next-offset-pos (last cells-to-move) offset)
          value-cell-after (cells pos-cell-after)]
      (when (= value-cell-after \.)
        [next-pos (-> (reduce
                       (fn [new-cells coords]
                         (assoc new-cells (next-offset-pos coords offset) (cells coords)))
                       cells
                       cells-to-move)
                      (assoc next-pos \.))]))))

(defn solve-p2 [[{:keys [cells]} moves]]
  (let [start-pos (ffirst (m/filter-vals (fn [v] (= v "@")) cells))
        final-cells (loop [pos start-pos
                           [move & moves] (take 5 moves)
                           cells (assoc cells start-pos \.)]
                      #_(println (draw pos cells))
                      (if-let [offset (offsets move)]
                        (let [next-pos (next-offset-pos pos offset)
                              next-value (cells next-pos)]
                          (case next-value
                            "#" (recur
                                 pos
                                 moves
                                 cells)
                            "." (recur
                                 next-pos
                                 moves
                                 cells)
                            ("[" "]") (if-let [p (push-p2 next-value next-pos offset cells)]
                                        (recur
                                         (first p)
                                         moves
                                         (second p))
                                        (recur
                                         pos
                                         moves
                                         cells))))

                        {:cells cells
                         :pos pos
                         :move move}))]
    final-cells #_(->> final-cells
                       (m/filter-vals (fn [v] (= v \O)))
                       (map (fn [[[y x]]]
                              (+ (* 100 y) x)))
                       (reduce +))))

(defn part-2
  [data]
  #_(println (draw nil cells))
  (solve-p2 (parser-p2 data)))

(part-2 input-p2-example)

(defn block? [[_ value]]
  (or (= value "]") (= value "[")))

(defn full-block [[pos value :as half-block]]
  (case value
    "[" [half-block [(next-offset-pos pos (offsets \>)) "]"]]
     "]" [[(next-offset-pos pos (offsets \<)) "["] half-block]))

(let [{:keys [cells pos move]}
      {:cells
       {[0 0] "#",
        [0 1] "#",
        [0 2] "#",
        [0 3] "#",
        [0 4] "#",
        [0 5] "#",
        [0 6] "#",
        [0 7] "#",
        [0 8] "#",
        [0 9] "#",
        [0 10] "#",
        [0 11] "#",
        [0 12] "#",
        [0 13] "#",
        [1 0] "#",
        [1 1] "#",
        [1 2] ".",
        [1 3] ".",
        [1 4] ".",
        [1 5] ".",
        [1 6] ".",
        [1 7] ".",
        [1 8] "#",
        [1 9] "#",
        [1 10] ".",
        [1 11] ".",
        [1 12] "#",
        [1 13] "#",
        [2 0] "#",
        [2 1] "#",
        [2 2] ".",
        [2 3] ".",
        [2 4] ".",
        [2 5] ".",
        [2 6] ".",
        [2 7] ".",
        [2 8] ".",
        [2 9] ".",
        [2 10] ".",
        [2 11] ".",
        [2 12] "#",
        [2 13] "#",
        [3 0] "#",
        [3 1] "#",
        [3 2] ".",
        [3 3] ".",
        [3 4] ".",
        [3 5] ".",
        [3 6] "[",
        [3 7] "]",
        [3 8] "[",
        [3 9] "]",
        [3 10] \.,
        [3 11] ".",
        [3 12] "#",
        [3 13] "#",
        [4 0] "#",
        [4 1] "#",
        [4 2] ".",
        [4 3] ".",
        [4 4] ".",
        [4 5] ".",
        [4 6] "[",
        [4 7] "]",
        [4 8] ".",
        [4 9] ".",
        [4 10] ".",
        [4 11] ".",
        [4 12] "#",
        [4 13] "#",
        [5 0] "#",
        [5 1] "#",
        [5 2] ".",
        [5 3] ".",
        [5 4] ".",
        [5 5] ".",
        [5 6] ".",
        [5 7] ".",
        [5 8] ".",
        [5 9] ".",
        [5 10] ".",
        [5 11] ".",
        [5 12] "#",
        [5 13] "#",
        [6 0] "#",
        [6 1] "#",
        [6 2] "#",
        [6 3] "#",
        [6 4] "#",
        [6 5] "#",
        [6 6] "#",
        [6 7] "#",
        [6 8] "#",
        [6 9] "#",
        [6 10] "#",
        [6 11] "#",
        [6 12] "#",
        [6 13] "#"},
       :pos [5 7],
       :move \^}]
     (let [offset (offsets move)]
       (letfn [(next-cell [pos]
                 (let [next-pos (next-offset-pos pos offset)
                       next-value (cells next-pos)]
                   [next-pos next-value]))
               (next-blocks [blocks]
                 (println blocks)
                 (reduce
                  (fn [linked-blocks [pos value]]
                    (let [cell (next-cell pos)]
                      (if (block? cell)
                        (concat linked-blocks (next-blocks (full-block cell)))
                        linked-blocks)))
                  [blocks]
                  blocks))]

         (println (draw pos cells))
         (let [cell (next-cell pos)]
           (println cell)
           (cond
             (block? cell) (let [parent (full-block cell)]
                             (next-blocks parent)))))))



;; # Tests
#_{:nextjournal.clerk/visibility {:code   :show
                                  :result :hide}}
#_(deftest test-2024-15
    (testing "part one"
      (is (= 1436690 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))



