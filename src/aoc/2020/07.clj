^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2020.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [instaparse.core :as insta]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map #(->> %
                  ((insta/parser
                    "<S> = bag <' bags contain '> contains*
                   bag = #'\\w+ \\w+'
                   number = #'[0-9]+'
                   contains = (number <' '> bag <#' bags?(\\.|(, ))'> | <'no other bags.'>)
                    "))
                  (insta/transform {:number   parse-int
                                    :bag      str
                                    :contains vector})))))

(def input (->> (slurp (io/resource "inputs/2020/07.txt"))
                parser))

{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."))

;; ## Part 1
(defn part-1
  [data]
  (let [raw-edges (->>
                   (for [[bag & contains] data]
                     (for [[number children-bag] contains]
                       (when children-bag
                         [bag children-bag number])))
                   (mapcat concat)
                   (remove nil?))
        g         (-> (uber/digraph)
                      (uber/add-edges* raw-edges))]

    #_(uber/pprint g)
    #_(alg/path-to g "shiny gold")
    #_(uber/viz-graph g)
    (uber/successors g "shiny gold")
    #_(-> (alg/shortest-path g {;; :start-nodes ["faded blue" "dotted black"]
                                :end-node "shiny gold"}))
        ;; :depths
        ;; count
        ;; dec
        

    #_(alg/all-destinations g)))





  ;
  


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input])

  ;
  



;; Tests
#_(deftest test-2020-16
    #_(testing "part one"
       (is (= 1 (part-1 input))))

    #_(testing "part two"
       (is (= 1 (part-2 input)))))

#_(part-1 input)
