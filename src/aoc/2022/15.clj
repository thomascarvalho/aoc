^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.15
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.string :as cs]
            [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [instaparse.core :as insta]
            [pp-grid.api :as g]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "15" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

(def input (->> (slurp (io/resource "inputs/2022/15.txt"))))

(def input-example "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(defn absolute-distance [s1 s2]
  (Math/abs (- s1 s2)))

(defn manhattan-distance [v1 v2]
  (->> (map absolute-distance v1 v2)
       (reduce +)))


;; Sensor at x=2885528, y=2847539: closest beacon is at x=2966570, y=2470834
(defn parser [d]
  (->> d
       ((insta/parser
         "S = <'Sensor at '> SENSOR <': closest beacon is at '> BEACON
          SENSOR = <'x='> INT <', y='> INT
          BEACON = <'x='> INT <', y='> INT
          INT = #'-?\\d+'
          "))
       #_{:clj-kondo/ignore [:unresolved-var]}
       (insta/transform {:INT    parse-int
                         :SENSOR vector
                         :BEACON vector
                         :S      (fn [sensor beacon]
                                   (hash-map sensor {:beacon beacon}))})))

(defn get-sensors [data]
  (keys data))

(defn get-beacons [data]
  (map :beacon (vals data)))

(defn create-grid [sensors beacons no-beacons]
  (let [assoc-data (fn [grid items c] (reduce (fn [acc k] (assoc acc k c)) grid items))]
    (-> (g/empty-grid)
        (assoc-data sensors "S")
        (assoc-data beacons "B")
        (assoc-data no-beacons "#")
        (g/box :left-padding 1 :right-padding 1))))

(def target-y-p1 2000000)

(defn get-no-beacons-p1 [[sx sy] beacon]
  (let [distance (manhattan-distance [sx sy] beacon)]
    (->> (range (- sx distance) (+ sx distance))
         (filter (fn [x] (<= (manhattan-distance [sx sy] [x target-y-p1]) distance)))
         (map (fn [x] [x target-y-p1]))
         set)))

(defn get-all-no-beacons-p1 [data sensors beacons]
  (->>
   data
   (mapcat (fn [[sensor {:keys [beacon]}]] (get-no-beacons-p1 sensor beacon)))
   (remove #(some #{%} (set (concat sensors beacons))))
   set))


(defn part-one []
  (let [data       (apply merge (map parser (cs/split-lines input)))
        sensors    (get-sensors data)
        beacons    (get-beacons data)
        no-beacons (get-all-no-beacons-p1 data sensors beacons)]
    (count no-beacons)))

;; PART 2 

(defn get-no-beacons-p2 [[sx sy] beacon]
  (let [distance (manhattan-distance [sx sy] beacon)]
    (->>
     (for [y    (range (- sy distance) (+ sy distance))
           x    (range (- sx distance) (+ sx distance))
           :let [cell [x y]]]
       (when (<= (manhattan-distance [sx sy] cell) distance)
         cell))
     (remove nil?)
     set)))

(defn get-all-no-beacons-p2 [data sensors beacons]
  (->>
   data
   (mapcat (fn [[sensor {:keys [beacon]}]] (get-no-beacons-p2 sensor beacon)))
   (remove #(some #{%} (set (concat sensors beacons))))
   set))



;; Tests & core



(defn part-two []
  (let [data input]
    0))

(deftest test-2022-15
  #_(testing "part one"
    (is (= 1 (part-one))))

  #_(testing "part two"
    (is (= 1 (part-two)))))


#_(let [data       (apply merge (map parser (cs/split-lines input-example)))
        sensors    (get-sensors data)
        beacons    (get-beacons data)
        no-beacons (get-all-no-beacons-p2 data sensors beacons)]
    (create-grid))

