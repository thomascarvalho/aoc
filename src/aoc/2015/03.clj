^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.03
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "03" "2015"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(def directions-mapping {">" [1 0]
                         "v" [0 1]
                         "<" [-1 0]
                         "^" [0 -1]})

(defn move [[x y] direction]
  (let [[x2 y2] (directions-mapping direction)]
    [(+ x x2) (+ y y2)]))

(defn parser [data]
  (-> data
      str/split-lines
      first
      (str/split #"")))

(def input (->> (slurp (io/resource "inputs/2015/03.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "^>v<"))

;; ## Part 1
(defn part-1
  [input-directions]
  (->>
   (loop [directions input-directions
          houses     [[0 0]]
          pos        [0 0]]
     (if (seq directions)
       (let [direction (first directions)
             new-pos   (move pos direction)]

         (recur (next directions) (conj houses new-pos) new-pos))
       houses))
   (frequencies)
   keys
   count)
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input-directions]
  (->>
   (loop [directions input-directions
          houses     [[0 0]]
          santa-pos  [0 0]
          robot-pos  [0 0]
          step       0]
     (if (seq directions)
       (let [santa?    (even? step)
             direction (first directions)
             new-pos   (move (if santa? santa-pos robot-pos) direction)]

         (recur
          (next directions)
          (conj houses new-pos)
          (if santa? new-pos santa-pos)
          (if santa? robot-pos new-pos)
          (inc step)))
       houses))
   (frequencies)
   keys
   count)

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; Tests
(deftest test-2015-03

  (testing "part one - example"
      (is (= 4 (part-1 (parser "^>v<")))))

  (testing "part one"
      (is (= 2592 (part-1 input))))

  
  
  (testing "part two - example"
    (is (= 11(part-2 (parser "^v^v^v^v^v")))))
  
  (testing "part two"
      (is (= 2360 (part-2 input)))))
