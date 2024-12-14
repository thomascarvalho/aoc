^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.25
  {:nextjournal.clerk/toc :collapsed}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution

(defn parser [data]
  (->> data
       u/to-lines
       (map (partial re-seq #"\w+"))
       (mapcat (fn [[s & rest]]
                 (mapv (partial vector s) rest)))))

(def input (->> (slurp (io/resource "inputs/2023/25.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr"))

;; ## Part 1
(defn part-1
  [edges]
  (let [g                (-> (uber/graph)
                             (uber/add-edges* (map sort edges)))
        zpotential-edges (->>
                          (alg/greedy-coloring g)
                          (sort-by val)
                          (filter #(-> % val zero?))
                          (mapcat (fn [[node _]]
                                    (map (fn [{:keys [src dest]}]
                                           [src dest]) (uber/in-edges g node)))))

        potential-edges  (->>
                          (for [ed (uber/nodes g)]
                            [ed (uber/in-degree g ed) (uber/out-degree g ed)])
                          (sort-by second >)
                          (take 4)
                          (mapcat (fn [[node _]]
                                    (map (fn [{:keys [src dest]}]
                                           [src dest]) (uber/in-edges g node)))))

        edges-to-remove  (->>
                          (for [e1    potential-edges
                                e2    potential-edges
                                e3    potential-edges
                                :when (and (not= e1 e2) (not= e1 e3) (not= e2 e3))]
                            (sort [e1 e2 e3]))
                          set
                          (into []))]

    ;; hfx/pzl, bvb/cmg,  nvd/jqt





    #_(alg/maximal-cliques g)
    #_(alg/degeneracy-ordering g)
    #_(uber/count-unique-edges g)
    #_(alg/connect g)

    #_(->> g
           (alg/greedy-coloring)
           (filter #(-> % val zero?)))

    #_(count edges-to-remove)

    (reduce (fn [_ [e1 e2 e3]]
              (let [filtered-edges (->> edges
                                        (remove #{e1})
                                        (remove #{e2})
                                        (remove #{e3})
                                        (remove #{(reverse e1)})
                                        (remove #{(reverse e2)})
                                        (remove #{(reverse e3)}))
                    comps          (-> (uber/graph)
                                       (uber/add-edges* filtered-edges)
                                       (alg/connected-components))]
                (if (= (count comps) 2)
                  (do
                    (reduced (* (-> comps first set count) (-> comps second set count))))
                  []))) [] edges-to-remove)))
  ;
  


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input])

  ;
  



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
#_(deftest test-2023-25
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

