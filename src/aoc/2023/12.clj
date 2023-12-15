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
               [(into [] (char-array t)) (u/parse-out-longs nums)]))))

(def input (->> (slurp (io/resource "inputs/2023/12.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "???.### 1,1,3
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

;; ## Part 1
(defn part-1
  [data]
  #_(println data)
  (->>
   (for [[initial-s nums] data]
     (loop [[n & rest] nums
            a          [initial-s]]
       (let [[s & groups] a]
         (if n
           (let [g (damaged-iterator s 0 n)]
             (if g
               (let [new-s (into []
                                 (->>
                                  (concat (subvec s 0 (first g))
                                          (subvec s (+ (first g) n) (count s)))
                                  (remove nil?)
                                  ))]
                 (println new-s)
                 (recur
                  rest
                  (-> a
                      (assoc 0 new-s)
                      (conj groups g))))
               (recur rest a)))
           a))))
   pprint)
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

(part-1 input-example)