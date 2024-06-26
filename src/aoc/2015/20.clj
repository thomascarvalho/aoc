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
                houses)res
               (reduce
                (fn [all [i h]]

                  (let [idx          (dec i)
                        [_ presents] (nth all idx)
                        new-count    (+ presents presents-counts)]
                    #_(when (zero? (mod idx 100)) (println new-count))
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
              res)))))
            clojure.pprint/pprint
            ))
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [input]
  
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-20
  #_(testing "part one"
    (is (= 1 (part-1 input))))

  #_(testing "part two"
    (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
