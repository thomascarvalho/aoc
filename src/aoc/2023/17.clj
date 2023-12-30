^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.17
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.core.matrix :as ma]
            [ubergraph.core :as uber]
            [clojure.set :refer [union]]
            [ubergraph.alg :as alg]
            [pathfinding :as pf]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "17" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       (u/to-matrix (fn [s] (-> s str parse-long)))
       pf/decode-matrix))

(def input (->> (slurp (io/resource "inputs/2023/17.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"))

(defn mget [m [y x]]
  (try
    (ma/mget m y x)
    (catch Exception _e nil)))

(def targets [[1 0] [-1 0] [0 1] [0 -1]])

(defn to-target-coords [[y x] [ty tx]]
  [(+ y ty) (+ x tx)])

(defn neighbors [cell grid]
  (let [[y x] cell]
    (filter #(grid %) [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])))


#_(defn bfs
  [grid start end]
  (loop [frontier  (conj clojure.lang.PersistentQueue/EMPTY start)
         came-from {start nil}
         visited   #{start}]
    (when (seq frontier)
      (let [current     (peek frontier)
            new-visited (conj visited current)]
        (if (= current end)
          (reverse (cons end (take-while #(not (nil? %)) (iterate came-from (came-from end)))))
          (let [next-neighbors (filter #(not (visited %))
                                       (neighbors current grid))]
            (recur
             (reduce conj (pop frontier) next-neighbors)
             (reduce #(assoc %1 %2 current) came-from next-neighbors)
             (union new-visited (set next-neighbors)))))))))

;; ## Part 1
(defn part-1
  [{:keys [cells width height]}]

  (let [max-y (dec height)
        max-x (dec width)

        ;; edges (for [current (ma/index-seq m)
        ;;             target  (map #(to-target-coords current %) targets)
        ;;             :let    [v (mget m target)]
        ;;             :when   v]
        ;;         [current target v])
        ;; g     (->
        ;;        (uber/digraph)
        ;;        (uber/add-edges* edges))
        ]

    #_(bfs cells [0 0] [max-y max-x])
    #_(alg/shortest-path g {:start-node  [0 0]
                            :end-node    [max-y max-x]
                            :cost-attr   :weight
                          ;; :traverse    true
                            :edge-filter (fn [e]
                                           (println e)
                                           e)
                          ;; :node-filter (fn [n]
                          ;;                (println n)
                          ;;                n
                          ;;                )
                            })
    




    #_(for [x (range 0 max-x)
            y (range 0 max-y)]))

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-17
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
#_(part-1 input-example)


(def ^:private inf (Long/MAX_VALUE))

(defn neighbors
  "Returns n's neighbors, optionally filtered if unvisited"
  ([g n] (get g n {}))
  ([g n uv] (select-keys (neighbors g n) uv)))

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [g costs curr unvisited]
  (let [curr-cost (costs curr)]
    (reduce
     (fn [c [nbr nbr-cost]] (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
     costs
     (neighbors g curr unvisited))))

(defn dijkstra
  "Returns a mapping of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a mapping of nodes to map of neighboring nodes and associated cost.
  Optionally, specify :target node to return only the min price for target"
  [g src & {:keys [target]}]
  (loop [costs     (assoc (zipmap (keys g) (repeat inf)) src 0)
         curr      src
         unvisited (disj (apply hash-set (keys g)) src)]
    (if (or (empty? unvisited) (= inf (costs curr)))
      costs
      (let [costs' (update-costs g costs curr unvisited)
            curr'  (first (sort-by costs' unvisited))]
        (if (= target curr)
          (costs' target)
          (recur costs'
                 curr'
                 (disj unvisited curr')))))))


(let [{:keys [cells height width]} input-example
      m                            (reduce (fn [m [pos v]]
                                             (let [[y x]     pos
                                                   neighbors (filter #(cells %) [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])]
                                               (assoc m pos (reduce (fn [ns p2]
                                                                      (assoc ns p2 (cells p2))) {} neighbors)))) {} cells)]

  (dijkstra m [0 0] {:target [(dec height) (dec width)]}))
