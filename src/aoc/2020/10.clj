^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2020.10
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "10" "2020"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map parse-int)))

(def input (->> (slurp (io/resource "inputs/2020/10.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"))

;; ## Part 1
(defn part-1
  [adapters]
  (let [my-adapter (+ (reduce max adapters) 3)]
    (->> (conj adapters my-adapter 0)
         sort
         (partition 2 1)
         (map (fn [[a1 a2]]
                (- a2 a1)))
         frequencies
         vals
         (reduce *))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input]
  
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2020-10
  (testing "part one"
    (is (= 3000 (part-1 input))))

  #_(testing "part two"
    (is (= 1 (part-2 input)))))

#_(part-1 input)