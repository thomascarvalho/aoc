^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.16
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.core.match :refer [match]]
            [pp-grid.api :as g]
            [clojure.set :refer [union difference]]
            [util :as u]
            [pathfinding :as pf]
            [clojure.test :refer :all]))


;; # Solution

(defn parser [data]
  (->> data
       u/to-matrix))

(def input (->> (slurp (io/resource "inputs/2023/16.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (->> (slurp (io/resource "inputs/2023/16-example.txt")) ;; Load the resource
                        parser))

(defn create-grid [energized]
  (let [assoc-data (fn [grid items c] (reduce (fn [acc k]
                                                (assoc acc (into [] (reverse k)) c)) grid (vec items)))]
    (-> (g/empty-grid)
        (assoc-data energized "#")
        (g/box :left-padding 1 :right-padding 1))))

(def dirs {:E [0 1]
           :W [0 -1]
           :N [-1 0]
           :S [1 0]})

(defn move [[[y x] d] [height width]]
  (let [[ty tx] (dirs d)
        new-x   (+ x tx)
        new-y   (+ y ty)]
    ;; To make it bounce, but that's not ok
    ;; (cond
    ;;   (>= new-y height) [[y x] :N]
    ;;   (neg? new-y) [[y x] :S]
    ;;   (>= new-x width) [[y x] :W]
    ;;   (neg? new-x) [[y x] :E]
    ;;   :else [[new-y new-x] d])
    (when-not (or
               (>= new-y height)
               (neg? new-y)
               (>= new-x width)
               (neg? new-x))
      [[new-y new-x] d])))

;; ## Part 1
(defn part-1
  [m]
  (let [{:keys [width height cells]} (pf/decode-matrix m)
        initial-beam                 [[0 0] (case (get cells [0 0])
                                              \. :E
                                              \/ :N
                                              \\ :S)]
        diffs-check                  3]

    (letfn [(move-beam [beams b]
              (let [beam (move b [height width])]
                (if beam
                  (let [[pos d] beam
                        v       (get cells pos)]
                    (letfn [(add
                              ([]
                               (add beam))
                              ([& rest]
                               (concat beams rest)))
                            (dir [d]
                              (assoc beam 1 d))]
                      (match [v d]
                        [\. _] (add)

                        [\| :E] (add (dir :N) (dir :S))
                        [\| :W] (add (dir :N) (dir :S))
                        [\| :N] (add)
                        [\| :S] (add)

                        [\- :E] (add)
                        [\- :W] (add)
                        [\- :N] (add (dir :E) (dir :W))
                        [\- :S] (add (dir :W) (dir :E))

                        [\/ :E] (add (dir :N))
                        [\/ :W] (add (dir :S))
                        [\/ :S] (add (dir :W))
                        [\/ :N] (add (dir :E))

                        [\\ :E] (add (dir :S))
                        [\\ :W] (add (dir :N))
                        [\\ :N] (add (dir :W))
                        [\\ :S] (add (dir :E))

                        :else (throw (Exception. (str "Bad match case : v: " v ", dir: " d))))))
                  beams)))]

      (->>
       (loop [beams     [initial-beam]
              energized #{(first initial-beam)}
              diffs     []]

         (if (and (>= (count diffs) diffs-check) (every? empty? diffs))
           energized
           (let [moved-beams   (reduce move-beam [] beams)
                 new-energized (union energized (set (map first moved-beams)))
                 diff          (difference new-energized energized)]
             (recur
              moved-beams
              new-energized
              (into [] (take-last diffs-check (conj diffs diff)))))))
       count)))
       ;

  #_(-> m
        process-contraption
        :tiles-energized
        count))
  ;


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input])

  ;


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
#_(deftest test-2023-16

    #_(testing "part one - example"
        (is (= 46 (part-1 input-example))))

    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

