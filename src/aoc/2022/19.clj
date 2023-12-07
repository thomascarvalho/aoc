(ns aoc.2022.19
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [util :refer  [parse-int]]
            [instaparse.core :as insta]))

(def input (->> (slurp (io/resource "inputs/2022/19.txt"))))

(defn parser [i]
  (->>
   ((insta/parser
     "BLUEPRINT = <'Blueprint '> ID <': '> INSTRUCTION*
            ID = INT
            INSTRUCTION = <'Each '> ROBOT <' costs '> COSTS <'.'>SPACE?
            ROBOT = RESOURCE <' robot'>
            RESOURCE = #'\\w+'
            COSTS = ((INT SPACE RESOURCE) |  (<' and '> INT SPACE RESOURCE))*
            INT = #'\\d+'
            <SPACE> = <' '>
               ") i)
   (insta/transform {:RESOURCE keyword
                     :INT parse-int
                     :INSTRUCTION (fn [[_ robot] [_ & costs]]
                                    [robot (vec costs)])
                     :BLUEPRINT (fn [[_ id] & robots-and-costs]
                                  [id (vec robots-and-costs)])})))

(def input-example "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")

(defn get-blueprint [id blueprints]
  (->> blueprints
       (filter #(= (first %) id))
       first))

(defn part-one []
  (let [data input]
    0))

(defn part-two []
  (let [data input]
    0))

(deftest test-2022-19
  (testing "part one"
    (is (= 1 (part-one))))

  (testing "part two"
    (is (= 1 (part-two)))))

#_(let [blueprints (map parser (cs/split-lines input-example))]
  (get-blueprint 1 blueprints)

  ;
  )

