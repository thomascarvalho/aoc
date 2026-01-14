(ns aoc.2025.11
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [ubergraph.core :as uber]
            [util :as u]
            [pathfinding :as pf]
            [clojure.set :as cset]
            [clojure.math.numeric-tower :as tower]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/11.txt"))))

(def input-example "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(defn parser [input]
  (->> input
       str/split-lines
       (mapcat
        (fn [l]
          (let [[input & outputs] (re-seq #"\w+" l)]
            (mapv
             (fn [o]
               [input o])
             outputs))))))

(defn part-one
  [data]
  (let [edges (parser data)
        g (-> (uber/digraph)
              (uber/add-edges* edges))]
    (pf/count-paths-dp g "you" "out")))

(def input-example-2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")


(defn part-two
  [data]
  (let [edges (parser data)
        g (-> (uber/digraph)
              (uber/add-edges* edges))
        paths-dac-then-fft (pf/count-paths-through g "svr" "out" ["dac" "fft"])
        paths-fft-then-dac (pf/count-paths-through g "svr" "out" ["fft" "dac"])]
    (+ paths-dac-then-fft paths-fft-then-dac)))

(deftest test-2025-11
  (testing "part one"
    (is (= 5 (part-one input-example)))
    (is (= 552 (part-one input))))

  (testing "part two"
    (is (= 2 (part-two input-example-2)))
    (is (= 307608674109300 (part-two input)))))



