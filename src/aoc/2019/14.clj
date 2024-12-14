^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2019.14
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution
(defn parser [data]
  (->> data
       str/split-lines
       (mapv (fn [l]
               (str/split l #" => ")))))

(def input (->> (slurp (io/resource "inputs/2019/14.txt")))) ;; Load the resource
                                         ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(defn decode-qty [line] 
  (->> (re-seq #"(\d+) (\w+)" line)
       (mapv (fn [[_ qty name]]
               {:qty qty
                :name name}))))


(let [data (->> input-example 
                parser
                (map (fn [[in out]]
                       {:in (decode-qty in)
                        :out (first (decode-qty out))})))]
  data)

;; ## Part 1
(defn part-1
  [input])

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input])

;; Tests
#_(deftest test-2019-14
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

