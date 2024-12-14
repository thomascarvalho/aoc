^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.05
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.test :refer :all]))

;; # Solution

(defn parser [data]
  (let [[[seeds] & blocks] (->> data
                                u/to-blocks
                                (mapv u/to-lines))]
    [(u/parse-out-longs seeds)
     (mapv (fn [[_ & nums]]
             (mapv #(->> % u/parse-out-longs (into [])) nums))
           blocks)]))

(def input (->> (slurp (io/resource "inputs/2023/05.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"))

(defn lookup [seed maps]
  (->
   (for [m     maps
         :let  [[dest source length] m
                index (- seed source)]
         :when (and (>= seed source) (< seed (+ source length)))]
     (+ dest index))
   first))

(defn get-seed-location [seed maps]
  (reduce
   (fn [value m]
     (or (lookup value m) value))
   seed
   maps))

;; ## Part 1
(defn part-1
  [[seeds maps]]
  (->> seeds
       (reduce
        (fn [acc seed]
          (min acc (get-seed-location seed maps))))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [[raw-seeds maps]]
  #_(let [all-seeds (map-indexed (fn [i d] [i d]) (partition 2 raw-seeds))
          agents    (map agent (repeat (count all-seeds) 0))]

      (doseq [[i [start-seed length]] all-seeds]
        (->>
         (range start-seed (+ start-seed length))
         (reduce
          (fn [acc seed]
            (min acc (get-seed-location seed maps))))
         (send (nth agents i) +)))

      (apply await agents)
      (reduce min (map deref agents)))
  (->> raw-seeds
       (partition 2)
       (pmap (fn [[start-seed length]]
               (let [seeds        (range start-seed (+ start-seed length))
                     mapped-seeds (pmap (fn [seed] (get-seed-location seed maps)) seeds)]
                 (reduce min Double/MAX_VALUE mapped-seeds))
               #_(->> (range start-seed (+ start-seed length))
                      (reduce (fn [acc seed]
                                (min acc (get-seed-location seed maps)))
                              Double/MAX_VALUE))))
       (reduce min Double/MAX_VALUE))
  #_(->> raw-seeds
         (partition 2)
         (reduce
          (fn [res [start-seed length]]
            (->>
             (range start-seed (+ start-seed length))
             (reduce
              (fn [acc seed]
                (min acc (get-seed-location seed maps))))
             (min res)))
          Double/MAX_VALUE)))

;; ## Suite
(deftest test-2023-05

  (testing "lookup"
    (is (= 81 (lookup 79 [[50 98 2] [52 50 48]]))))

  (testing "part one -example"
    (is (= 35 (part-1 input-example))))

  (testing "part one"
    (is (= 226172555 (part-1 input))))

  (testing "part two - example"
    (is (= 46 (time (part-2 input-example))))))

;; ## Results

;; (def sums (map agent (repeat 10 0)))

;; (def numbers (range 1000000)) ;; one million numbers

;; ;; loop through all numbers and round-robin the agents
;; (doseq [[x agent] (map vector numbers (cycle sums))]
;;   (send agent + x))

;; ;; wa it at most 10 seconds
;; #_(apply await-for 10000 sums)

;; ;; sum up the answers in all ten agents
;; (println (apply + (map deref sums)))

#_(part-2 input)
