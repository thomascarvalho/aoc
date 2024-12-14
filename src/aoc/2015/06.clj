^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.06
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map #(let [[_ action y1 x1 y2 x2] (re-find #"(turn (?:on|off)|toggle) (\d+),(\d+) through (\d+),(\d+)" %)]
               [(keyword (str/replace action " " "-"))
                (mapv parse-long [y1 x1])
                (mapv parse-long [y2 x2])]))))

(def input (->> (slurp (io/resource "inputs/2015/06.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))



;; ## Part 1
(defn part-1
  [all-actions]
  (->>
   (loop [[action & actions] all-actions
          lights             {}]

     (if-not action
       lights
       (let [[t [y1 x1] [y2 x2]] action
             cells               (for [y (range y1 (inc y2))
                                       x (range x1 (inc x2))]
                                   [y x])]
         (recur
          actions
          (reduce (fn [m pos]
                    (case t
                      :toggle (let [v (m pos)]
                                (if (= v :on)
                                  (assoc m pos :off)
                                  (assoc m pos :on)))
                      :turn-on (assoc m pos :on)
                      :turn-off (assoc m pos :off))) lights cells)))))

   vals
   frequencies
   :on))
  


;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [all-actions]
  (->>
   (loop [[action & actions] all-actions
          lights             {}]

     (if-not action
       lights
       (let [[t [y1 x1] [y2 x2]] action
             cells               (for [y (range y1 (inc y2))
                                       x (range x1 (inc x2))]
                                   [y x])]
         (recur
          actions
          (reduce (fn [m pos]
                    (let [v (or (m pos) 0)]
                      (case t
                        :toggle (assoc m pos (+ v 2))
                        :turn-on (assoc m pos (inc v))
                        :turn-off (if (= v 0)
                                    m
                                    (assoc m pos (dec v)))))) lights cells)))))

   vals
   (reduce +)))

#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-06
  (testing "part one"
    (is (= 377891 (part-1 input))))

  (testing "part two"
    (is (= 14110788 (part-2 input)))))

