^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.index
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

   8  {:stars 2}
   9  {:stars 2}
   10 {:stars 2}
   11 {:stars 2}

   12 {:stars 1}
   13 {:stars 2}
   16 {:stars 2}

   18 {:stars 2}
   19 {:stars 1}

   21 {:stars 2}
   23 {:stars 2}
   25 {:stars 1}})

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(u/list-problems 2015 (days))


