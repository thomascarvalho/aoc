^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.21
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "21" "2015"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/parse-out-longs
       (zipmap [:hit-points :damage :armor])))

(def input (->> (slurp (io/resource "inputs/2015/21.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "Hit Points: 12
Damage: 7
Armor: 2"))

(def shop-weapons [["Dagger"        8     4       0]
                   ["Shortsword"   10     5       0]
                   ["Warhammer"    25     6       0]
                   ["Longsword"    40     7       0]
                   ["Greataxe"     74     8       0]])

(def shop-armors
  [["None" 0 0 0]
   ["Leather"      13     0       1]
   ["Chainmail"    31     0       2]
   ["Splintmail"   53     0       3]
   ["Bandedmail"   75     0       4]
   ["Platemail"   102     0       5]])

(def shop-rings
  [["None 1" 0 0 0]
   ["None 2" 0 0 0]
   ["Damage +1"    25     1       0]
   ["Damage +2"    50     2       0]
   ["Damage +3"   100     3       0]
   ["Defense +1"   20     0       1]
   ["Defense +2"   40     0       2]
   ["Defense +3"   80     0       3]])

(defn take-damage [attacker defender]
  (update defender :hit-points - (max (- (:damage attacker) (:armor defender)) 1)))

(defn get-starting-combinaisons []
  (for [[_ weapon-gold weapon-damage _]                                                         shop-weapons
        [_ armor-gold _ armor]                                                                  shop-armors
        [[_ ring-1-gold ring-1-damage ring-1-armor] [_ ring-2-gold ring-2-damage ring-2-armor]] (combo/combinations shop-rings 2)]
    {:hit-points 100
     :damage     (+ weapon-damage ring-1-damage ring-2-damage)
     :armor      (+ armor ring-1-armor ring-2-armor)
     :gold       (+ weapon-gold armor-gold ring-1-gold ring-2-gold)}))

(defn fight [me opponent]
  (loop [me       me
         opponent opponent
         me?      true]
    (if me?
      (let [{:keys [hit-points]
             :as   new-opponent} (take-damage me opponent)]
        (if (<= hit-points 0)
          {:wins? true
           :gold  (:gold me)}
          (recur me new-opponent (not me?))))

      (let [{:keys [hit-points]
             :as   new-me} (take-damage opponent me)]
        (if (<= hit-points 0)
          {:wins? false
           :gold  (:gold me)}
          (recur new-me opponent (not me?)))))))

;; ## Part 1
(defn part-1
  [opponent]
  (->>
   (get-starting-combinaisons)
   (map #(fight % opponent))
   (filter #(-> % :wins?))
   (sort-by :gold)
   first
   :gold))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [opponent]
  (->>
   (get-starting-combinaisons)
   (map #(fight % opponent))
   (filter #(-> % :wins? (= false)))
   (sort-by :gold >)
   first
   :gold))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-21
  (testing "part one"
    (is (= 91 (part-1 input))))

  (testing "part two"
    (is (= 158 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results