^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.20
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "20" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
(defn parser [d]
  (->> d
       str/split-lines
       (map parse-int)
       vec))
;; First things first, let's load our input and parse it
(def input (->> (slurp (io/resource "inputs/2022/20.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}



;;  Example
(def input-example (parser "1
2
-3
3
-2
0
4"))

(defn move-element [numbers index displacement]
  (let [vec-len   (count numbers)
        new-index (mod (+ index displacement) vec-len)
        element   (nth numbers index)]
    (cond
      (= index new-index) [numbers new-index]
      :else (let [removed (vec (concat (subvec numbers 0 index) (subvec numbers (inc index))))]
              [(vec (concat (subvec removed 0 new-index) [element] (subvec removed new-index))) new-index]))))



;; ## Part 1
(defn part-1
  [input-numbers]
  (loop [numbers       input-example
         current-index 0
         step          1]
    (if (= step 4)
      numbers
      (let [move                       (nth numbers current-index)
            [new-numbers target-index] (move-element numbers current-index move)]
         ;; TODO: get correct current index
        (recur new-numbers (if (> target-index current-index) current-index (inc current-index)) (inc step))))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]
  nil)

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; # Tests
#_(deftest test-2022-20
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

#_(part-1 input-example)
