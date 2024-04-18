^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.03
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "03" "2017"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (-> data
      (str/replace #"\n" "")
      parse-long))

(def input (->> (slurp (io/resource "inputs/2017/03.txt"))
                parser))

{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))

(def dirs [[1 0]
           [0 1]
           [-1 0]
           [0 -1]])

;; ## Part 1
(defn part-1
  [data]
  (->>
   (loop [idx               1
          pos               [0 0]
          [dir & next-dirs] (cycle dirs)
          step              1
          last-step         nil]
     (if (>= idx data)
       pos
       (let [new-step          (if (and last-step (= last-step step))
                                 (inc step)
                                 step)
             [new-pos new-idx] (reduce
                                (fn [[pos idx] dir]
                                  (let [idx (inc idx)
                                        pos (mapv + pos dir)]
                                    (if (= idx data)
                                      (reduced [pos idx])
                                      [pos idx])))
                                [pos idx]
                                (repeat step dir))]
         (recur
          new-idx
          new-pos
          next-dirs
          new-step
          step))))
   (u/manhattan-distance [0 0])))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}


(defn sum-adjacents [pos cells]
  (reduce (fn [t coords]
            (+ t (get cells (mapv + pos coords) 0))) 0 [[1 0] [0 1] [-1 0] [0 -1] [1 1] [-1 1] [-1 -1] [1 -1]]))


(defn part-2
  [data]
  (loop [idx               1
         pos               [0 0]
         [dir & next-dirs] (cycle dirs)
         step              1
         last-step         nil
         cells             {[0 0] 1}]
    (let [new-step (if (and last-step (= last-step step))
                     (inc step)
                     step)
          r        (reduce
                    (fn [[[pos idx] cells] dir]
                      (let [idx (inc idx)
                            pos (mapv + pos dir)
                            t   (sum-adjacents pos cells)]
                        (if (> t data)
                          (reduced t #_[[pos idx] (assoc cells pos t)])
                          [[pos idx] (assoc cells pos t)])))
                    [[pos idx] cells]
                    (repeat step dir))]
      (if (number? r)
        r
        (let [[[new-pos new-idx] cells] r]
          (recur
           new-idx
           new-pos
           next-dirs
           new-step
           step
           cells))))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-03
  (testing "part one"
    (is (= 475 (part-1 input))))

  (testing "part two"
    (is (= 279138 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(t/test-render #'test-2017-03)