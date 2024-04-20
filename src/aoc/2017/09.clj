^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.09
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "09" "2017"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data))

(def input (->> (slurp (io/resource "inputs/2017/09.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))

(defn trim-group [s]
  (str/join "" (-> s
                   (subs 1)
                   (drop-last))))

(let [data "{{<!>},{<!>},{<!>},{<a>}}"] 
  
  (let [garbages (re-seq #"\<.*?[^!]\>" data)]
    garbages
    )
  
  )

;; ## Part 1
(defn part-1
  [data]
  data)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-09
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/test-render #'test-2017-09)