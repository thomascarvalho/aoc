^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.20
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "20" "2015"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines))

(def input (->> (slurp (io/resource "inputs/2015/20.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))

#_(let [expected-max #_150 36000000
        houses (atom (into [] (take 1000000 (repeat 0))))]
    (loop [elf-idx 1]
      (let [max-presents (apply max @houses)]
        (if (> max-presents expected-max)
          (ffirst (filter (fn [[_i v]]
                            (>= v expected-max)) (map-indexed (fn [i v] [i v]) @houses)))
          (let [[_ & elf-houses] (take 10 (iterate (partial + elf-idx) 0))]
            (doseq [current-house elf-houses]
              (swap! houses update current-house + (* 10 elf-idx)))
            (recur
             (inc elf-idx)))))))

#_(time (let [expected-max #_150 36000000
              houses (sorted-map) #_(into [] (take 1000000 (repeat 0)))]
          (loop [houses houses
                 elf-idx 1]
            (let [[_ & elf-houses] (take 10 (iterate (partial + elf-idx) 0))
                  r (reduce (fn [houses current-house]
                              (let [n (+ (get houses current-house 0) (* 10 elf-idx))]
                                (if (and (>= n expected-max) (>= elf-idx current-house))
                                  (reduced [current-house (assoc houses current-house n)])
                                  (assoc houses current-house n)))) houses elf-houses)]
              (if (vector? r)
                (let [[current-max-house new-houses] r]
                  [current-max-house elf-idx]
                 (ffirst (filter (fn [[i v]]
                                   (>= v expected-max)) (into (sorted-map) new-houses))))
                (recur
                 r
                 (inc elf-idx)))))))

;; to high : 1272600
;; to high : 2708996


;; ## Part 1
(defn part-1
  [data]
  (let [houses         (take 5000 (repeat 0))
        indexed-houses (map-indexed (fn [i v]
                                      [(inc i) v]) houses)
        elves          (take 5000 (range 1 Double/MAX_VALUE))]

    (->>
     (loop [[elf & next-elves] elves
            houses             indexed-houses]
       (if-not elf
         (let [m (reduce (fn [t [_ p]]
                           (max t p)) 0 houses)]
           {:max  m
            :diff (- 36000000 m)})
         (let [presents-counts (* elf 10)
               selected-houses #_(take 10000)
               (filter
                (fn [[i]]
                  (zero? (mod i elf)))
                houses) res
               (reduce
                (fn [all [i h]]

                  (let [idx          (dec i)
                        [_ presents] (nth all idx)
                        new-count    (+ presents presents-counts)]
                    (if (>= new-count 36000000)
                      (reduced {;;:all (take 10 all)
                                :i        i
                                :presents new-count})
                      (u/assoc-at all idx [i new-count]))))
                houses
                selected-houses)]

           (if (map? res)
             res

             (recur
              next-elves
              res))))))))
     

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input])


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
#_(deftest test-2015-20
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

