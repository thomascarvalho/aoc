^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.index
  (:require [util :as u]))

;; # 2022

{:nextjournal.clerk/visibility {:code   :hide
                                :result :hide}}
(defn days []
  {1  {:stars 2}
   2  {:stars 2}
   3  {:stars 2}
   4  {:stars 2}
   5  {:stars 2}
   6  {:stars 2}
   7  {:stars 2}
   8  {:stars 2}
   9  {:stars 1}
   10 {:stars 2}
   11 {:stars 1}
   12 {:stars 2}
   13 {:stars 2}
   14 {:stars 1}
   15 {:stars 1}
   17 {:stars 1}
   21 {:stars 1}
   22 {:stars 1}
   23 {:stars 2}})


{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(u/list-problems 2022 (days))