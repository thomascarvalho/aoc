^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2016.01
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.set :refer [intersection union]]
            [util :as u]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "01" "2016"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->>
   data
   (re-seq #"(\w)(\d+)")
   (map (fn [[_ dir step]]
          [(keyword dir) (parse-long step)]))))

(def input (->> (slurp (io/resource "inputs/2016/01.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

(def initial-dirs [:N :E :S :W])

(def dir->coord {:N [-1 0]
                 :E [0 1]
                 :S [1 0]
                 :W [0 -1]})

(defn turn [directions rotate]
  (->
   (case rotate
     :R (drop 1 (take 5 (cycle directions)))
     :L (concat (take-last 1 directions) (take 3 directions)))
   vec))

(defn move [initial-coords steps directions]
  (let [[x2 y2] (dir->coord (first directions))]
    (loop [[final-x final-y] initial-coords
           current-steps     steps
           path              #{}]
      (if (zero? current-steps)
        [[final-x final-y] path]
        (let [new-cell [(+ final-x x2) (+ final-y y2)]]
          (recur
           new-cell
           (dec current-steps)
           (conj path new-cell)))))))

;;  Example
(def input-example (parser "R8, R4, R4, R8"))

;; ## Part 1
(defn part-1
  [data]
  (->>
   data
   (reduce (fn [{:keys [pos dirs]} [rotate steps]]
             (let [new-dirs  (turn dirs rotate)
                   [new-pos] (move pos steps new-dirs)]
               {:pos  new-pos
                :dirs new-dirs}))

           {:pos  [0 0]
            :dirs initial-dirs})
   :pos
   (u/manhattan-distance [0 0])))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  (->>
   data
   (reduce (fn [{:keys [pos dirs path]} [rotate steps]]
             (let [new-dirs           (turn dirs rotate)
                   [new-pos new-path] (move pos steps new-dirs)
                   inter              (intersection path new-path)]

               (if (seq inter)
                 (reduced {:pos (first inter)})
                 {:pos  new-pos
                  :dirs new-dirs
                  :path (union path new-path)})))

           {:pos  [0 0]
            :dirs initial-dirs
            :path #{[0 0]}})
   :pos
   (u/manhattan-distance [0 0])))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2016-01
  (testing "part one"
    (is (= 287 (part-1 input))))

  (testing "part two"
    (is (= 133 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results