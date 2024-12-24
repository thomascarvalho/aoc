^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2024.index
  (:require [util :as u]
            [aoc.2024.07]
            [aoc.2024.06]
            [aoc.2024.05]
            [aoc.2024.04]
            [aoc.2024.03]
            [aoc.2024.02]
            [aoc.2024.01]))

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
   9  {:stars 2}
   10 {:stars 2}
   11 {:stars 1}
   12 {:stars 1}
   13 {:stars 0}
   14 {:stars 2}
   15 {:stars 1}
   16 {:stars 1}
   17 {:stars 2}
   18 {:stars 2}
   19 {:stars 1}
   20 {:stars 1}
   21 {:stars 0}
   22 {:stars 2}
   23 {:stars 1}
   24 {:stars 1}})

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(u/list-problems 2024 (days))
