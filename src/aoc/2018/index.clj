^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.index
  (:require [util :as u]))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :hide}}

(defn days []
  {1 {:stars 2}
   3 {:stars 2}
   4 {:stars 1}
   5 {:stars 2}
   7 {:stars 1}
   10 {:stars 2}
   11 {:stars 1}
   18 {:stars 1}})

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(u/list-problems 2018 (days))
