^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.index
  (:require [util :as u]))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :hide}}

(defn days []
  {1  {:stars 2}
   2  {:stars 2}
   3  {:stars 2}
   4  {:stars 1}
   5  {:stars 2}
   6  {:stars 1}
   7  {:stars 2}
   8  {:stars 1}
   9  {:stars 1}

   11 {:stars 2}
   12 {:stars 1}
   13 {:stars 2}
   14 {:stars 1}
   15 {:stars 2}})

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(u/list-problems 2021 (days))