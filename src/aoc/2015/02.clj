^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.02
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution

;; 4x23x21
;; 2*l*w + 2*w*h + 2*h*l
(defn parser [data]
  (->> data
       str/split-lines
       (map #(map parse-int (str/split % #"x")))))

(def input (->> (slurp (io/resource "inputs/2015/02.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "2x3x4"))

;; ## Part 1
(defn part-1
  [boxes]
  (->>
   (for [[l w h] boxes
         :let    [lw (* l w)
                  wh (* w h)
                  hl (* h l)]]
     (->
      (* 2 lw)
      (+ (* 2 wh))
      (+ (* 2 hl))
      (+ (min lw hl wh))))
   (reduce +)))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [boxes]
  (->>
   (for [[l w h] boxes
         :let    [sorted (sort [l w h])]]
     (->
      (* 2 (first sorted))
      (+ (* 2 (second sorted)))
      (+ (* l h w))))
   (reduce +)))
  ;
  



;; Tests
(deftest test-2015-02
  (testing "part one - example"
    (is (= 58 (part-1 (parser "2x3x4")))))

  (testing "part one"
    (is (= 1598415 (part-1 input))))


  (testing "part two - example "
    (is (= 34 (part-2 (parser "2x3x4")))))

  (testing "part two"
    (is (= 3812909 (part-2 input)))))
