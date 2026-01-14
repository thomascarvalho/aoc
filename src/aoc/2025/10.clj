(ns aoc.2025.10
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.set :as cset]
            [util :as u]
            [clojure.math.numeric-tower :as tower]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/10.txt"))))

(def input-example "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defn parser [input]
  (->> input
       str/split-lines
       (map (fn [l] (let [[lights & others] (str/split l #" ")
                          lights-target (reduce (fn [m [i c]]
                                                  (assoc m i (case c
                                                               \. 0
                                                               \# 1)))
                                                {}
                                                (map-indexed vector (to-array (drop 1 (drop-last lights)))))]
                      {:lights lights-target
                       :buttons (mapv u/parse-out-longs-v (drop-last others))
                       :joltage (u/parse-out-longs-v (last others))})))))

(defn reset-lights [lights]
  (update-vals lights (fn [_] 0)))

(defn toggle [v]
  (if (zero? v) 1 0))

(defn push-button [current-lights button]
  (reduce
   (fn [acc i]
     (update acc i toggle))
   current-lights
   button))

(defn push
  [buttons lights target]
  (let [count-from (fn count-from [path lights step]
                     (reduce
                      (fn [acc button]
                       (let [r (push-button lights button)
                             path (conj path [button r])]
                         (cond
                           (= r target) (reduced {:success path})
                           (>= step 6) nil
                           :else
                           (apply conj acc (count-from path r (inc step))))))
                      path
                      buttons))]

    (count-from [] lights 0)))

(defn process [{:keys [lights buttons]}]
  (let [current-lights (reset-lights lights)]
    (->> (push buttons current-lights lights)
         (filter (fn [[info ]] (= :success info)))
         (map second)
         (map count)
         (reduce min))))

(defn part-one
  [data]
  (let [lines (parser data)]
    (->> (map process lines)
         (reduce +))))

(defn push-p2
  [buttons lights target]
  (let [count-from (fn count-from [path lights step]
                     (reduce
                      (fn [acc button]
                       (let [r (push-button lights button)
                             path (conj path [button r])]
                         (cond
                           (= r target) (reduced {:success path})
                           (>= step 12) nil
                           :else
                           (apply conj acc (count-from path r (inc step))))))
                      path
                      buttons))]

    (count-from [] lights 0)))

(defn process-joltage [{:keys [joltage buttons]}]
  (let [current-lights (reset-lights joltage)]
    (->> (push-p2 buttons current-lights joltage)
         (filter (fn [[info ]] (= :success info)))
         (map second)
         (map count)
         (reduce min))))

(defn part-two
  [data]
  (let [lines (parser data)]
    (->> (map process-joltage lines)
         (reduce +))))

#_(deftest test-2025-10
    (testing "part one"
      (is (= 50 (part-one input-example)))
      (is (= 4790063600 (part-one input))))

    (testing "part two"
      (is (= 40 (part-two input-example)))
      (is (= 1393669447690 (part-two input)))))

(time (part-two input-example))

