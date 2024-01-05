^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.16
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "16" "2015"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map #(u/instaparse  % "S = <'Sue '> num <': '> things
                                   things = (thing <': '> num <', '?>)*
                                   thing = #'\\w+'
                                   num = #'\\d+'"
                            {:num    parse-long
                             :S      vector
                             :things hash-map
                             :thing  keyword}))))

(def input (->> (slurp (io/resource "inputs/2015/16.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

(def expected-things {:children    3
                      :cats        7
                      :samoyeds    2
                      :pomeranians 3
                      :akitas      0
                      :vizslas     0
                      :goldfish    5
                      :trees       3
                      :cars        2
                      :perfumes    1})

;; ## Part 1
(defn part-1
  [aunts]
  (reduce (fn [t [num categories]]
            (when (every? true?
                          (reduce-kv (fn [r cat n]
                                       (conj r (= (get expected-things cat) n))) [] categories))
              (reduced num))) []  aunts))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [aunts]
  (reduce (fn [t [num categories]]
            (when (every? true?
                          (reduce-kv (fn [r cat n]
                                       (conj r (case cat
                                                 (:trees :cats) (> n (get expected-things cat))
                                                 (:pomeranians :goldfish) (< n (get expected-things cat))
                                                 (= (get expected-things cat) n)))) [] categories))
              (reduced num))) []  aunts)

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
(deftest test-2015-16
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

(part-2 input)