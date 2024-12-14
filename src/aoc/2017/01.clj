^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.01
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (str/replace data "\n" ""))

(def input (->> (slurp (io/resource "inputs/2017/01.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "91212129"))

;; ## Part 1
(defn part-1
  [data]
  (let [s (str data (first (take 1 data)))]
    (reduce (fn [t [a b]]
              (if (= a b)
                (+ t (parse-long (str a)))
                t)) 0 (partition 2 1 s))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [s    data
        step (/ (count s) 2)]
    (->> (map-indexed (fn [i x] [i x]) s)
         (reduce (fn [t [i a]]
                   (if (= a (nth (str s s) (+ i step)))
                     (+ t (parse-long (str a)))
                     t)) 0))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-01
  (testing "part one"
    (is (= 1341 (part-1 input))))

  (testing "part two"
    (is (= 1348 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

