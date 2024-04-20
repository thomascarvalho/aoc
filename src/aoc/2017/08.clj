^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.08
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "08" "2017"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map (fn [s]
              (str/split s #" ")))))

(def input (->> (slurp (io/resource "inputs/2017/08.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"))

;; ## Part 1
(defn part-1
  [data]
  (let [instructions (->> data
                          (map (fn [[reg op num _ target comp val]]
                                 [reg
                                  (case op
                                    "inc" +
                                    "dec" -)
                                  (parse-long num)
                                  target
                                  (case comp
                                    ">" >
                                    "<" <
                                    "==" =
                                    ">=" >=
                                    "<=" <=
                                    "!=" not=)
                                  (parse-long val)])))]
    (->>
     instructions
     (reduce (fn [m [reg op num target comp val]]

               (if (comp (get m target 0) val)
                 (assoc m reg (op (get m reg 0) num))
                 m)) {})
     vals
     (apply max))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [instructions (->> data
                          (map (fn [[reg op num _ target comp val]]
                                 [reg
                                  (case op
                                    "inc" +
                                    "dec" -)
                                  (parse-long num)
                                  target
                                  (case comp
                                    ">" >
                                    "<" <
                                    "==" =
                                    ">=" >=
                                    "<=" <=
                                    "!=" not=)
                                  (parse-long val)])))]
    (->>
     instructions
     (reduce (fn [[m v-max] [reg op num target comp val]]

               (if (comp (get m target 0) val)
                 (let [v (op (get m reg 0) num)]
                   [(assoc m reg v) (max v v-max)])
                 [m v-max])) [{} 0])
     second)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-08
  (testing "part one"
    (is (= 4416 (part-1 input))))

  (testing "part two"
    (is (= 5199 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(t/test-render #'test-2017-08)