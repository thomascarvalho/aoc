^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.16
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.core.matrix :as ma]
            [clojure.core.match :refer [match]]
            [pp-grid.api :as g]
            [clojure.set :refer [difference]]
            [util :as u]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "16" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-matrix))

(def input (->> (slurp (io/resource "inputs/2023/16.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (->> (slurp (io/resource "inputs/2023/16-example.txt")) ;; Load the resource
                        parser))

(defn create-grid [energized]
  (let [assoc-data (fn [grid items c] (reduce (fn [acc k] (assoc acc k c)) grid items))]
    (-> (g/empty-grid)
        (assoc-data energized "#")
        (g/box :left-padding 1 :right-padding 1))))

(def dirs {:E [1 0]
           :W [-1 0]
           :N [0 -1]
           :S [0 1]})

(defn move [{:keys [pos dir]
             :as   beam} {:keys [max-x max-y]}]
  (let [[x y]   pos
        [tx ty] dir
        new-x   (+ x tx)
        new-y   (+ y ty)]
    (when-not (or
               (> new-y max-y)
               (neg? new-y)
               (> new-x max-x)
               (neg? new-x))
      (assoc beam :pos [new-x new-y]))))

(defn action [m {:keys [pos dir]
                 :as   b}]
  (let [[x y]         pos
        [dir-x dir-y] dir
        cell          (ma/mget m y x)
        vb            [b]]

    (match [cell dir-x dir-y]
      [\. _ _] vb

      [\| _ 0] [(assoc b :dir (dirs :N))
                (assoc b :dir (dirs :S))]
      [\| 0 _] vb

      [\- _ 0] vb
      [\- 0 _] [(assoc b :dir (dirs :E))
                (assoc b :dir (dirs :W))]

      [\/ 1 0] [(assoc b :dir (dirs :N))]
      [\/ -1 0] [(assoc b :dir (dirs :S))]
      [\/ 0 1] [(assoc b :dir (dirs :W))]
      [\/ 0 -1] [(assoc b :dir (dirs :E))]

      [\\ 1 0] [(assoc b :dir (dirs :S))]
      [\\ -1 0] [(assoc b :dir (dirs :N))]
      [\\ 0 1] [(assoc b :dir (dirs :E))]
      [\\ 0 -1] [(assoc b :dir (dirs :W))]

      :else (throw (Exception. (str "Bad match case : cell: " cell ", dir-x: " dir-x ", dir-y: " dir-y))))))


(defn process-contraption [m]
  (let [limits             {:max-x (dec (count (first m)))
                            :max-y (dec (count m))}
        check-diffs-number 30]
    (loop [beams     [{:pos [0 0]
                       :dir (dirs :E)}]
           energized #{[0 0]}
           diffs     []]
      (if (and (>= (count diffs) check-diffs-number) (every? #(= % #{}) diffs))
        {:beams           beams
         :tiles-energized energized}
        (let [moved-beams   (->>
                             beams
                             (map #(move % limits))
                             (remove nil?))
              new-energized (apply conj energized (mapv :pos moved-beams))
              new-beams     (mapcat #(action m %) moved-beams)
              diff          (difference new-energized energized)]
          (recur new-beams new-energized (into [] (take-last check-diffs-number (conj diffs diff)))))))))


;; ## Part 1
(defn part-1
  [m]
  (-> m
      process-contraption
      :tiles-energized
      count)
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
(deftest test-2023-16

  #_(testing "moves "
      (let [limits {:max-x 9
                    :max-y 9}]
        (is (= nil (move {:pos [9 0]
                          :dir [1 0]} limits)))
        (is (= {:pos [2 0]
                :dir [1 0]} (move {:pos [1 0]
                                   :dir [1 0]} limits)))
        (is (= nil (move {:pos [0 0]
                          :dir [-1 0]} limits)))
        (is (= {:pos [0 0]
                :dir [-1 0]} (move {:pos [1 0]
                                    :dir [-1 0]} limits)))
        (is (= {:pos [0 1]
                :dir [0 1]} (move {:pos [0 0]
                                   :dir [0 1]} limits)))
        (is (= nil (move {:pos [0 0]
                          :dir [0 -1]} limits)))))

  (testing "part one - example"
    (is (= 46 (part-1 input-example))))

  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

#_(part-1 input)

;; 7736 -> too high
