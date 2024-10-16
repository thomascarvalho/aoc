^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.05
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "05" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it


(def input (-> (slurp (io/resource "inputs/2018/05.txt"))
               u/to-lines
               first)) ;; Load the resource
                                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example  "dabAcCaCBAcCcaDA")

(def reg #"([a-zA-Z])(?!\1)(?i:\1)")


;; ## Part 1
(defn part-1
  [data]
  (->
   (loop [s data]
     (let [new-s (str/replace-first s reg "")]
       (if (= new-s s)
         s
         (recur new-s))))
   count))


;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  (->> (for [letter (map str (distinct (str/lower-case data)))
             :let [s (-> data
                         (str/replace letter "")
                         (str/replace (str/upper-case letter) ""))]]
         (part-1 s))
       (apply min)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2018-05
  (testing "part one"
    (is (= 11476 (part-1 input))))

  (testing "part two"
    (is (= 5446 (part-2 input)))))



