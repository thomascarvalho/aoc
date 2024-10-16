^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.02
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "02" "2018"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it
(def input (->> (slurp (io/resource "inputs/2018/02.txt")) ;; Load the resource
                str/split-lines))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (str/split-lines "abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab"))

;; ## Part 1
(defn part-1
  [input]
  (let [freqs         (map frequencies input)
        grouped-freqs (map #(-> % first set) (group-by vals freqs))
        data          (map #(let [has-two?   (contains? % 2)
                                  has-three? (contains? % 3)]
                              [(if has-two? 1 0) (if has-three? 1 0)]) grouped-freqs)]
    (* (reduce + 0 (map first data)) (reduce + 0 (map second data)))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]
  nil)

(let [d (map #(let [se (-> % first set)]
                 [(if (se 2) 1 0) (if (se 3) 1 0)]) (group-by vals (map frequencies input)))]
     (* (reduce + 0 (map first d)) (reduce + 0 (map second d))))
     

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2018-02
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

#_(part-1 input)
