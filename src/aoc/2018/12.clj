^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.12
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u] 
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "12" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (let [[initial-state-row _ & rows] (u/to-lines data)]
    {:initial-state (str/split (str/replace-first initial-state-row #"initial state: " "") #"")
     :notes (reduce (fn [m l] 
                      (let [[from to] (str/split l #" => ")]
                        (assoc m (str/split from #"") to))) {} rows)}))

    
       

(def input (->> (slurp (io/resource "inputs/2018/12.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"))

#_(let [{:keys [initial-state notes]} input-example]
    (loop [cur-state (into [] (concat ["." "."] initial-state ["." "."]))
           step 0]
      (let [combs (into [] (map-indexed (fn [x i] [x (into [] i)]) (partition 5 1 cur-state)))]
        (reduce (fn [s [i comb]]
                  (if-let [v (get notes comb)]
                    (assoc s (+ i 2) v)
                    s)) cur-state combs)
        #_combs)))
      

;; ## Part 1
(defn part-1
  [data]
  data)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2018-12
  #_(testing "part one"
     (is (= 1 (part-1 input))))

  #_(testing "part two"
     (is (= 1 (part-2 input)))))

