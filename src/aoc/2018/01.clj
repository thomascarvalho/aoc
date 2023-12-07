^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.01
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "01" "2018"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

(defn parser
  [s]
  (->> s
       str/split-lines
       (map #(str/replace % #"[ \+]" ""))
       (map parse-int)))

;; # Solution
;;
;; First things first, let's load our input and parse it
(def input (parser (slurp (io/resource "inputs/2018/01.txt"))))

{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "+1
                            -2
                            +3
                            +1"))

;; ## Part 1
(defn part-1
  [input]
  (reduce + input))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]
  (loop [numbers      (cycle input)
         current-freq 0
         freqs        #{}]
    (let [n        (first numbers)
          new-freq (+ current-freq n)]
      (if (contains? freqs new-freq)
        new-freq
        (recur (next numbers) new-freq  (conj freqs new-freq))))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)

{:nextjournal.clerk/visibility {:code   :hide
                                :result :hide}}
;; Tests
(deftest test-2018-01
  (testing "part one"
    (is (= 516 (part-1 input))))

  (testing "part two"
    (is (= 71892 (part-2 input)))))