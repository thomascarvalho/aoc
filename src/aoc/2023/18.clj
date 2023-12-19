^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.18
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [test-util :as t]
            [pp-grid.api :as g]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "18" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it
;; [y x]
(def dirs {"D" [0 1]
           "U" [0 -1]
           "R" [1 0]
           "L" [-1 0]})

(defn create-grid [dig-plan]
  (let [assoc-data (fn [grid items c] (reduce (fn [acc k] (assoc acc (vec k) c)) grid items))]
    (-> (g/empty-grid)
        (assoc-data dig-plan "#"))))

(defn parser [data]
  (->> data
       str/split-lines
       (map (fn [s]
              (let [[dir step color] (str/split s #" ")]
                {:dir   (dirs dir)
                 :step  (parse-long step)
                 :color (subs color 1 (dec (count color)))})))))

(def input (->> (slurp (io/resource "inputs/2023/18.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"))

(defn move-path [[x y] {:keys [dir step]}]
  (reduce (fn [path s]
            (let [[tx ty] dir]
              (conj path [(+ x (* tx s)) (+ y (* ty s))]))) [] (range 1 (inc step))))


;; (defn intersects? [p1 p2 x y]
;;   (let [[x1 y1] p1
;;         [x2 y2] p2
;;         min-y   (min y1 y2)
;;         max-y   (max y1 y2)]
;;     (and (not= y1 y2)
;;          (>= y min-y)
;;          (<= y max-y)
;;          (let [x-coord (if (= x1 x2)
;;                          x1
;;                          (+ x1 (/ (* (- y y1) (- x2 x1)) (- y2 y1))))]
;;            (> x-coord x)))))

;; (defn inside-polygon? [x y path]
;;   (let [extended-path (conj path (first path))
;;         intersections (count (filter (fn [[p1 p2]] (intersects? p1 p2 x y)) (partition 2 1 extended-path)))]
;;     (odd? intersections)))

;; (defn get-grid-size [path]
;;   (let [max-x (apply max (map first path))
;;         max-y (apply max (map second path))]
;;     [(inc max-x) (inc max-y)]))

;; (defn fill-polygon [path]
;;   (let [[cols rows] (get-grid-size path)
;;         points      (for [x     (range cols)
;;                           y     (range rows)
;;                           :when (inside-polygon? x y path)] [x y])]
;;     (set points)))

(defn segments-on-line [y path]
  (filter #(let [[[x1 y1] [x2 y2]] %]
             (or (= y y1) (= y y2)))
          (partition 2 1 (conj path (first path)))))

(defn fill-line [y path cols]
  (let [segments (segments-on-line y path)
        xs       (sort (mapcat #(vector (first %) (first (next %))) segments))] ; Extraire les coordonnées x des segments
    (if (empty? xs)
      #{}
      (reduce (fn [acc x]
                (if (contains? acc x)
                  (disj acc x)
                  (conj acc x)))
              #{}
              (range (apply min (map first xs)) (apply max (map first xs))))))) ; Utiliser min et max pour déterminer la plage


(defn fill-polygon [path cols rows]
  (set (for [y (range rows)
             x (fill-line y path cols)]
         [x y])))

;; ## Part 1
(defn part-1
  [actions]
  (let [{:keys [dig-plan]} (reduce (fn [r action]
                                     (let [path (move-path (:pos r) action)]
                                       (-> r
                                           (update :dig-plan concat path)
                                           (assoc :pos (last path)))))
                                   {:dig-plan []
                                    :pos      [0 0]} actions)
        [width height] (->> dig-plan
                            (reduce (fn [[max-x max-y] [x y]]
                                      [(max max-x x) (max max-y y)]) [0 0])
                            (mapv inc)
                            )]
    
    (->>
     (fill-polygon dig-plan width height)
     (concat dig-plan)
     set
     count)
    ;
    ))



  ;


;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input])

  ;


;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-18
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

(part-1 input)

;; 35853 -> too high



#_(-> (walk-based-fill (map reverse path)))

; Appel de la fonction
#_(walk-based-fill path)


; Exemple d'utilisation
#_(fill-polygon (map reverse path))