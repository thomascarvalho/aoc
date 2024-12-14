^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.19
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [pathfinding :as pf]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map char-array)
       pf/decode-matrix
       :cells
       (reduce (fn [m [pos c]]
                 (if (= c \space)
                   m
                   (assoc m pos c))) {})))

(def input (->> (slurp (io/resource "inputs/2017/19.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ "))

(defn start-pos [cells]
  (->> cells
       (filter (fn [[[y _] c]]
                 (and (zero? y) (= c \|))))
       ffirst))

(defn letter? [c]
  (re-matches #"[A-Z]+" (str c)))

;; ## Part 1
(defn part-1
  [data]
  (let [cells         data
        coords        (map first cells)
        [max-y max-x] (reduce (fn [[y-max x-max] [y x]]
                                [(max y-max y) (max x-max x)]) [0 0] coords)]
    (->>
     (loop [pos  (start-pos cells)
            dir  :D
            path []]
       (let [[y x]         pos
             current-range (case dir
                             :D (map (fn [y] [y x]) (range (inc y) (inc max-y)))
                             :U (map (fn [y] [y x]) (range (dec y) 0 -1))
                             :R (map (fn [x] [y x]) (range (inc x) (inc max-x)))
                             :L (map (fn [x] [y x]) (range (dec x) 0 -1)))
             new-path      (->> current-range
                                (reduce (fn [line-path pos]
                                          (let [c (get cells pos)]
                                            (if-not c
                                              (reduced line-path)
                                              (if (and (not= c \|) (not= c \-))
                                                (conj line-path [pos c])
                                                line-path)))) []))]
          ;; (prn new-path)
          ;; (prn dir)
         (if (seq new-path)
           (let [[last-pos last-c] (last new-path)]
             (if (= last-c \+)
               (let [[y x]   last-pos
                     new-dir (case dir
                               (:D :U) (let [right (get cells [y (inc x)])
                                             left  (get cells [y (dec x)])]
                                         #_(prn right)
                                         (if (or (letter? right) (= right \-))
                                           :R
                                           (if (or (letter? left) (= left \-))
                                             :L
                                             nil)))

                               (:L :R) (let [up   (get cells [(dec y) x])
                                             down (get cells [(inc y) x])]
                                         (if (or (letter? down) (= down \|))
                                           :D
                                           (if (or (letter? up) (= up \|))
                                             :U
                                             nil))))]
                 (recur
                  last-pos
                  new-dir
                  (concat path new-path)))
               (concat path new-path)))
           path)))
     (map second)
     (filter letter?)
     (str/join ""))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn part-2
  [data]
  (let [cells         data
        coords        (map first cells)
        [max-y max-x] (reduce (fn [[y-max x-max] [y x]]
                                [(max y-max y) (max x-max x)]) [0 0] coords)]
    (->>
     (loop [pos   (start-pos cells)
            dir   :D
            path  []
            steps 1]
       (let [[y x]         pos
             current-range (case dir
                             :D (map (fn [y] [y x]) (range (inc y) (inc max-y)))
                             :U (map (fn [y] [y x]) (range (dec y) 0 -1))
                             :R (map (fn [x] [y x]) (range (inc x) (inc max-x)))
                             :L (map (fn [x] [y x]) (range (dec x) 0 -1)))
             new-path      (->> current-range
                                (reduce (fn [line-path pos]
                                          (let [c (get cells pos)]
                                            (if-not c
                                              (reduced line-path)
                                              (if (and (not= c \|) (not= c \-))
                                                (conj line-path [pos c])
                                                line-path)))) []))]
         (if (seq new-path)
           (let [[last-pos last-c] (last new-path)
                 new-steps         (u/manhattan-distance pos last-pos)]
             (if (= last-c \+)
               (let [[y x]   last-pos
                     new-dir (case dir
                               (:D :U) (let [right (get cells [y (inc x)])
                                             left  (get cells [y (dec x)])]
                                         #_(prn right)
                                         (if (or (letter? right) (= right \-))
                                           :R
                                           (if (or (letter? left) (= left \-))
                                             :L
                                             nil)))

                               (:L :R) (let [up   (get cells [(dec y) x])
                                             down (get cells [(inc y) x])]
                                         (if (or (letter? down) (= down \|))
                                           :D
                                           (if (or (letter? up) (= up \|))
                                             :U
                                             nil))))]
                 (recur
                  last-pos
                  new-dir
                  (concat path new-path)
                  (+ steps new-steps)))
               (+ steps new-steps)))
           steps))))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-19
  (testing "part one"
    (is (= "SXWAIBUZY" (part-1 input))))

  (testing "part two"
    (is (= 16676 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

