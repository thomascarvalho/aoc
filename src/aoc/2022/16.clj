^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.16
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.test :refer :all]
            [instaparse.core :as insta]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "16" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;

(defn parser [input]
  (->> input
       str/split-lines
       (map (fn [d]
              (->> d

                   ((insta/parser
                     " S = <'Valve '> valve <' has flow rate='> flow-rate <'; '> tunnels
            int = #'-?\\d+'  
            valve = #'[A-Z]{2}'
            <flow-rate> = int
            <tunnels> = (<'tunnels lead to valves '> (valve <', '>?)+) | (<'tunnel leads to valve '> valve)
"))
                   #_{:clj-kondo/ignore [:unresolved-var]}
                   (insta/transform {:int   parse-int
                                     :S     (fn [valve flow-rate & tunnels-to]
                                              {:valve      valve
                                               :flow-rate  flow-rate
                                               :tunnels-to tunnels-to})
                                     :valve str}))))
       (reduce (fn [all {:keys [valve flow-rate tunnels-to]}]
                 (->> tunnels-to
                      (map #(vector valve % flow-rate))
                      (concat all))) [])))

;; First things first, let's load our input and parse it
(def input (->> (slurp (io/resource "inputs/2022/16.txt")) ;; Load the resource
                parser))                             ;; Split into lines


;;  Example
(def input-example (parser "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"))


;; ## Part 1
(defn part-1
  [raw-egdes]
  (let [g (uber/graph)]
    (-> g
        (uber/add-edges* raw-egdes))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]
  nil)

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2022-16
  #_(testing "part one"
    (is (= 1 (part-1 input))))

  #_(testing "part two"
    (is (= 1 (part-2 input)))))

#_(part-1 input-example)