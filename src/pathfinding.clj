(ns pathfinding
  (:require [clojure.set :refer [union]]))

(def grid [[1 2 3]
           [4 5 6]
           [7 8 9]])

(defn decode-matrix
  "Get :width, :height and :cells {[y x] value, [y2 x2] value2 ... }"
  [m]
  {:width  (count (first m))
   :height (count m)
   :cells  (->>
            (for [[y row] (map-indexed vector m)
                  [x v]   (map-indexed vector row)]
              (sorted-map [y x] v))
            (apply merge))})

(defn neighbors [cell grid]
  (let [[y x] cell]
    (filter #(and (grid %) #_(not= (grid %) 0))
            [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])))

(defn bfs
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

#_(let [g (decode-matrix grid)]
  (time (bfs (:cells g) [0 0] [2 2]))

  #_(loop [queue [[[0 0] 1]]]

      queue
      #_(let [[]])
      #_(dissoc queue [0 0])))


