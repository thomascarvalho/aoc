(ns aoc.2024.19
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.math.combinatorics :as combo]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (let [[towels _ & designs] (->> data u/to-lines)]
    {:towels (into #{} (str/split towels #", "))
     :designs designs}))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/19.txt"))
                parser))

(def input-example (parser "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"))

;; Logic

(defn get-valid-designs [towels designs]
  (let [pattern (re-pattern (str "^(" (str/join "|" towels) ")+$"))]
    (for [design designs
          :when (re-matches pattern design)]
      design)))

;; ## Part 1
(defn part-1
  [{:keys [towels designs]}]
  (count (get-valid-designs towels designs)))

;; ## Part 2

(defn get-groups [towels design index]
  (let [result (transient [])]
    (loop [i index]
      (if (<= i (count design))
        (let [s (subs design index i)]
          (when (towels s)
            (conj! result (cons s (get-groups towels design i))))
          (recur (inc i)))
        (persistent! result)))))

(defn group-to-path
  ([tree] (group-to-path tree []))
  ([tree path]
   (if (not (sequential? tree))
     [(conj path tree)]
     (let [[towel & children] tree]
       (if (empty? children)
         [(conj path towel)]
         (mapcat #(group-to-path % (conj path towel)) children))))))

(defn get-paths-count [towels design]
  (let [groups (get-groups towels design 0)]
    (->> groups
         (mapcat (fn [g] (filter #(> (count %) 1) (group-to-path g))))
         count)))

(defn part-2
  [{:keys [towels designs]}]
  (let [designs (get-valid-designs towels designs)]
    (->> designs
         #_(take 1)
         (mapv #(get-paths-count towels %))
         (reduce +))))

;; # Tests
#_{:nextjournal.clerk/visibility {:code   :show
                                  :result :hide}}
(deftest test-2024-19
  (testing "part one"
    (is (= 6 (part-1 input-example)))
    (is (= 296 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

