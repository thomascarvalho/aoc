^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2020.index
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

   7  {:stars 1}
   8  {:stars 1}
   9  {:stars 2}
   10 {:stars 1}
   11 {:stars 2}})

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(u/list-problems 2020 (days))

