(ns every.17
  (:require [clojure.test :refer [deftest is testing]]
            [ubergraph.alg :as alg]
            [ubergraph.core :as uber]
            [clojure.string :as str]
            [util :as u]
            [clojure.math.combinatorics :as combo]
            [medley.core :as m]
            [pathfinding :as pf]))

(defn parser [data]
  (-> data
      u/to-matrix
      pf/decode-matrix))


(def input (parser "*.........*.........*
.....................
.*...........*...*...
.....................
..*.......*......*...
.....................
........*...*........
.....................
*.....*...*...*.....*
.....................
........*...*........
.....................
....*.......*...*....
.....................
....*....*.......*...
.....................
*.........*.........*"))

(def input-example (parser "*...*
..*..
.....
.....
*.*.."))

#_(let [{:keys [cells]} input-example
        stars (reduce
               (fn [stars [coords v]]
                 (if (= v \*)
                   (conj stars coords)
                   stars))
               []
               cells)]
    (->> (combo/permutations stars)
         (reduce
            (fn [all combs]
                (conj all (reduce
                              (fn [t [c1 c2]]
                                  (+ t (u/manhattan-distance c1 c2)))
                              0 
                              (partition 2 1 combs)))) 
            [])))
       
       




