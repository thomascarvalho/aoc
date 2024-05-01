^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.index
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
   8  {:stars 2}

   10 {:stars 2}
   11 {:stars 2}
   12 {:stars 2}
   13 {:stars 2}
   14 {:stars 2}
   15 {:stars 2}
   16 {:stars 2}
   17 {:stars 1}
   18 {:stars 1}
   19 {:stars 2}})

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(u/list-problems 2017 (days))