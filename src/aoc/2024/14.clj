(ns aoc.2024.14
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [pp-grid.api :as g]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (->> data
       u/to-lines
       (mapv (fn [line]
               (mapv #(into [] %) (partition 2 (u/parse-out-longs line)))))))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/14.txt"))
                parser))

(def input-example (parser "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"))

;; Logic
(defn draw [points [max-x max-y]]
  (->> points
       frequencies
       (reduce (fn [acc [[x y] c]]
                 (assoc acc [x y] (str c)))
               (-> (g/empty-grid)
                   (assoc [-1 -1] \.)
                   (assoc [-1 max-y] \.)
                   (assoc [max-x -1] \.)
                   (assoc [max-x max-y] \.)))))

(defn move [spaces coords offset]
  (mapv #(mod %1 %2) (mapv + coords offset) spaces))

;; ## Part 1
(defn part-1
  [data]
  (let [initial-robots data
        seconds 100
        spaces [101 103]
        move (partial move spaces)
        [middle-x middle-y] (mapv #(int (/ % 2)) spaces)
        robots (loop [step 0
                      robots initial-robots]
                 (if (>= step seconds)
                   (mapv first robots)
                   (recur
                    (inc step)
                    (reduce (fn [acc [pos offset]]
                              (conj acc [(move pos offset) offset]))
                            []
                            robots))))]

    (->> robots
         (filter (fn [[x y]]
                   (and (not= x middle-x)
                        (not= y middle-y))))
         (group-by (fn [[x y]]
                     [(< x middle-x)
                      (> x middle-x)
                      (< y middle-y)
                      (> y middle-y)]))
         (mapv (comp count second))
         (reduce *))))

;; ## Part 2
(defn part-2
  [data]
  (let [initial-robots data
        spaces [101 103]
        move (partial move spaces)]
    (loop [seconds 0
           robots initial-robots]
      (if (and (> seconds 5000) (every? #(= % 1) (vals (frequencies (mapv first robots)))))
        (do
         #_(println (draw (mapv first robots) spaces)) 
         seconds)
        (recur
         (inc seconds)
         (reduce (fn [acc [pos offset]]
                   (conj acc [(move pos offset) offset]))
                 []
                 robots))))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-14
  (testing "part one"
    (is (= 230900224 (part-1 input))))

  (testing "part two"
    (is (= 6532 (part-2 input)))))
