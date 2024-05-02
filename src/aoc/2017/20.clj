^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.20
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
(clerk/html (u/load-problem "20" "2017"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (mapv #(->> % u/parse-out-longs
                   (partition 3)
                   (into [])))))

(def input (->> (slurp (io/resource "inputs/2017/20.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"))

(def input-example-2 (parser "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>    
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"))


;; ## Part 1
(defn part-1
  [data]
  (->>
   (loop [particules data
          step       1000]
     (if (zero? step)
       particules
       (recur
        (->> particules
             (reduce (fn [arr [pos vel acc]]
                       (let [new-vel (mapv + vel acc)]
                         (conj arr [(mapv + pos new-vel) new-vel acc]))) []))
        (dec step))))
   (map first)
   (map-indexed (fn [i v] [i v]))
   (map (fn [[i v]]
          [i (reduce + 0 (mapv (fn [p]
                                 (abs p)) v))]))
   (sort-by second <)
   ffirst))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn part-2
  [data]
  (->>
   (loop [particules data
          step       500]
     (if (zero? step)
       particules
       (let [new-particules (->> particules
                                 (reduce (fn [arr [pos vel acc]]
                                           (let [new-vel (mapv + vel acc)]
                                             (conj arr [(mapv + pos new-vel) new-vel acc]))) []))
             uniques-pos    (->> new-particules
                                 (map first)
                                 frequencies
                                 (filter (fn [[_ n]]
                                           (= n 1)))
                                 (map first))]

         (recur
          (->> new-particules
               (filter (fn [[pos]]
                         (some #{pos} uniques-pos))))
          (dec step)))))
   count))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-20
  (testing "part one"
    (is (= 144 (part-1 input))))

  (testing "part two"
    (is (= 477 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(t/test-render #'test-2017-20)