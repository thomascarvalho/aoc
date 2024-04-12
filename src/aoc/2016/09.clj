^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2016.09
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u] 
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "09" "2016"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines))

(def input (->> (slurp (io/resource "inputs/2016/09.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "X(8x2)(3x3)ABCY"))

(let [data  (first input)
      groups (u/re-pos #"\((\d+)x(\d+)\)" data)]

(println groups)  
  (loop [res ""
         start  0
         [current & groups] groups]
    
    (if (and current (< start (count data)))
      (let [[idx-group group]  current
            [n-chars n-repeat] (u/parse-out-longs group)
            group-length       (count group)

            repeat-start       (+ idx-group group-length)
            repeat-end         (+ idx-group group-length n-chars)
            _                  (prn repeat-end)
            next-group-idx     (if (seq groups)
                                 (first (first groups))
                                 (inc (count data)))
            
            _                  (prn next-group-idx)

            next-str           (if (and next-group-idx (< repeat-end next-group-idx))
                                 (subs data repeat-end (dec next-group-idx))
                                 "")]
        

        (recur (str
                res
                (subs data start idx-group)
                (str/join "" (repeat n-repeat (subs data repeat-start repeat-end)))
                next-str)
               (+ idx-group group-length n-chars)
               groups))
      
      (count res)
    


      )

    
    
    )
  
  
  )

;; ## Part 1
(defn part-1
  [data]
  data
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
(deftest test-2016-09
  #_(testing "part one"
    (is (= 1 (part-1 input))))

  #_(testing "part two"
    (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

(part-1 input-example)