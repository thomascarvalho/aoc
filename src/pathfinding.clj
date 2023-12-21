(ns pathfinding)

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

(defn- sort-queue [queue]
  (sort-by second queue))

(let [g (decode-matrix grid)]

  (loop [queue [[[0 0] 1]]]
    
    queue
    #_(let [[]])
    #_(dissoc queue [0 0])))


