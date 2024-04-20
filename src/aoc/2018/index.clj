^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.index
  (:require [util :as u]))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :hide}}

(defn days []
  {1 {:stars 2}})

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(u/list-problems 2018 (days))