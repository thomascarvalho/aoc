(ns pathfinding
  (:require [clojure.set :refer [union]]
            [ubergraph.core :as uber]
            [pp-grid.api :as pg]))

(def grid [[1 2 3]
           [4 5 6]
           [7 8 9]])

(defn decode-matrix
  "Get :width, :height and :cells {[y x] value, [y2 x2] value2 ... }"
  ([m]
   (decode-matrix identity m))
  ([decode-val-fn m]
   {:width  (count (first m))
    :height (count m)
    :cells  (->>
             (for [[y row] (map-indexed vector m)
                   [x v]   (map-indexed vector row)]
               (sorted-map [y x] (decode-val-fn v)))
             (apply merge))}))

(defn neighbors [cell grid]
  (let [[y x] cell]
    (filter #(and (grid %))
            [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])))

(defn bfs
  ([grid start end]
   (bfs grid start end neighbors))
  ([grid start end neighbors-fn]
   (loop [frontier  (conj clojure.lang.PersistentQueue/EMPTY start)
          came-from {start nil}
          visited   #{start}]
     (when (seq frontier)
       (let [current     (peek frontier)
             new-visited (conj visited current)]
         (if (= current end)
           (reverse (cons end (take-while #(not (nil? %)) (iterate came-from (came-from end)))))
           (let [next-neighbors (filter #(not (visited %))
                                        (neighbors-fn current grid))]
             (recur
              (reduce conj (pop frontier) next-neighbors)
              (reduce #(assoc %1 %2 current) came-from next-neighbors)
              (union new-visited (set next-neighbors))))))))))

(defn draw-grid
  [cells & {:keys [reverse?]}]
  (let [assoc-data (fn [grid items] (reduce (fn [acc [k v]]
                                              (assoc acc (if reverse?
                                                           (into [] (reverse k))
                                                           k) (str v))) grid items))]
    (-> (pg/empty-grid)
        (assoc-data cells) #_(assoc-data cells)
        #_(pg/box :left-padding 1 :right-padding 1))))

(defn find-all-paths
  "Trouve tous les chemins possibles entre start-node et end-node dans un graphe orienté"
  [graph start-node end-node]
  (let [paths (atom [])
        find-paths
        (fn find-paths [current-node current-path visited]
          (if (= current-node end-node)
            (swap! paths conj current-path)
            ;; Utilisation de successors pour un graphe orienté
            (doseq [neighbor (uber/successors graph current-node)]
              (when-not (visited neighbor)
                (find-paths neighbor
                            (conj current-path neighbor)
                            (conj visited neighbor))))))]
    (find-paths start-node [start-node] #{start-node})
    @paths))

(defn find-all-paths-optimized
  "Trouve tous les chemins possibles entre start-node et end-node dans un graphe non orienté,
   avec une limite de profondeur pour éviter une explosion combinatoire"
  [graph start-node end-node & {:keys [max-depth max-paths] :or {max-depth 20 max-paths 1000}}]
  (let [paths (atom [])
        ;; Utilisation d'une PriorityQueue pour favoriser les chemins les plus courts
        queue (java.util.PriorityQueue.
               (comparator (fn [a b]
                             (compare (count (second a))
                                      (count (second b))))))]

    ;; On commence avec le chemin initial
    (.add queue [start-node [start-node] #{start-node}])

    (while (and (not (.isEmpty queue))
                (< (count @paths) max-paths)) ; Limite le nombre total de chemins
      (let [[current-node current-path visited] (.poll queue)]
        (if (= current-node end-node)
          (swap! paths conj current-path)
          ;; Continue seulement si on n'a pas atteint la profondeur maximale
          (when (< (count current-path) max-depth)
            (doseq [neighbor (uber/neighbors graph current-node)]
              (when-not (visited neighbor)
                (.add queue
                      [neighbor
                       (conj current-path neighbor)
                       (conj visited neighbor)])))))))

    ;; Trie les chemins par longueur et limite le nombre de résultats
    (->> @paths
         (sort-by count)
         (take 100) ; Limite à 100 chemins
         vec)))
