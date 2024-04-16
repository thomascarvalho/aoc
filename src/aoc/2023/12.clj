^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.12
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [test-util :as t]
            [clojure.pprint :refer [pprint]]
            [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "12" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map #(let [[t nums] (str/split % #" ")]
               [t (u/parse-out-longs nums)]))))

(def input (->> (slurp (io/resource "inputs/2023/12.txt"))
                parser))
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser
                    "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"))

(defn damaged-iterator [t from length]

  (let [r (->> (partition length 1 t)
               (map-indexed (fn [i g] [i g]))
               (filter (fn [[i group]] (every? #(= % \#) group))))]
    (when (= 1 (count r)) (first r))))

(let [[l n] ["???.###" '(1 1 3)]]

  (u/re-pos #"\#+" l))

;; ## Part 1
(defn part-1
  [data]
  (for [d    data
        :let [[s nums] d
              groups (map (fn [n]
                            [n (->
                                (str "\\.?(#{" n "})\\.?")
                                re-pattern
                                (re-seq s))]) nums)]]
    (do
      (cons groups d)))

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-12
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

#_(part-1 input-example)