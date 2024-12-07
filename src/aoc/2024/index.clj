^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2024.index
  (:require [util :as u]))

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
   8  {:stars 0}
   9  {:stars 0}
   10 {:stars 0}
   11 {:stars 0}
   12 {:stars 0}
   15 {:stars 0}
   18 {:stars 0}
   19 {:stars 0}
   21 {:stars 0}
   23 {:stars 0}})

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(u/list-problems 2024 (days))
