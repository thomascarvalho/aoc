^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.15
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
  (->> data
       u/to-lines
       (map #(let [[all name] (re-matches #"(\w+):.*" %)]
               (hash-map
                name
                (zipmap [:capacity :durability :flavor :texture :calories] (u/parse-out-longs all)))))))

(def input (->> (slurp (io/resource "inputs/2015/15.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;; ## Part 1
(defn part-1
  [input]
  (let [nb-ingredients 4
        teespoons (->> (for [a (range 1 (- 101 nb-ingredients))
                             b (range 1 (- 101 nb-ingredients a))
                             c (range 1 (- 101 nb-ingredients a b))
                             :let [d (- 100 a b c)]
                             :when (pos? d)]
                         [a b c d])
                       set)]
    (reduce (fn [v spoons]
              (let [{:keys [capacity durability flavor texture]}
                    (reduce (fn [t [i n]]
                              (let [{:keys [capacity durability flavor texture calories]} (first (vals (nth input i)))]
                                (-> t
                                    (update :capacity + (* capacity n))
                                    (update :durability + (* durability n))
                                    (update :flavor + (* flavor n))
                                    (update :texture + (* texture n)))))
                            {:capacity 0
                             :durability 0
                             :flavor 0
                             :texture 0
                             :calories 0}
                            (map-indexed (fn [i s] [i s]) spoons))
                    total (* (if (pos? capacity)
                               capacity
                               0)
                             (if (pos? durability)
                               durability
                               0)
                             (if (pos? flavor)
                               flavor
                               0)
                             (if (pos? texture)
                               texture
                               0))]
                (if (> total v)
                  total
                  v)))
            0 teespoons)))


;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input]
  (let [nb-ingredients 4
        teespoons (->> (for [a (range 1 (- 101 nb-ingredients))
                             b (range 1 (- 101 nb-ingredients a))
                             c (range 1 (- 101 nb-ingredients a b))
                             :let [d (- 100 a b c)]
                             :when (pos? d)]
                         [a b c d])
                       set)]
    (reduce (fn [v spoons]
              (let [{:keys [capacity durability flavor texture calories]}
                    (reduce (fn [t [i n]]
                              (let [{:keys [capacity durability flavor texture calories]} (first (vals (nth input i)))]
                                (-> t
                                    (update :capacity + (* capacity n))
                                    (update :durability + (* durability n))
                                    (update :flavor + (* flavor n))
                                    (update :texture + (* texture n))
                                    (update :calories + (* calories n)))))
                            {:capacity 0
                             :durability 0
                             :flavor 0
                             :texture 0
                             :calories 0}
                            (map-indexed (fn [i s] [i s]) spoons))
                    total (* (if (pos? capacity)
                               capacity
                               0)
                             (if (pos? durability)
                               durability
                               0)
                             (if (pos? flavor)
                               flavor
                               0)
                             (if (pos? texture)
                               texture
                               0))]
                (if (and (= calories 500) (> total v))
                  total
                  v)))
            0 teespoons)))



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-15
  (testing "part one"
    (is (= 222870 (part-1 input))))

  (testing "part two"
    (is (= 117936 (part-2 input)))))

