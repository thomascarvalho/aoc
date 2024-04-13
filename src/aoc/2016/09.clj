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
(def input-example (parser "(27x3)ZFPNOWLAEAEMVDZHFYHXDUVOFWJ(6x3)SBKNUX(26x2)JEAITUGDSJCXZBKGKMKEQKTZCN(21x1)LYEAWHPDVHFAAZNAZJRFF(254x15)(77x8)(2x8)BK(13x13)J"))

(let [data           #_"A(2x2)BCD(2x2)EFG" (first input)
      groups         (u/re-pos #"\((\d+)x(\d+)\)" data)
      ;; groups-indexed (map-indexed (fn [i v] [i v]) groups)
      ]

  (->
   (loop [res           ""
          current-index 0
          group-index   0]

     (prn res)
     (if-let [g (try
                  (nth groups group-index)
                  (catch Exception e
                    nil))]
       (let [[idx-group group]  g
             [n-chars n-repeat] (u/parse-out-longs group)
             group-length       (count group)

             _                  (prn idx-group)
             _                  (prn group-length)
             _                  (prn n-chars)

             repeat-start       (+ idx-group group-length)
             repeat-end         (+ idx-group group-length n-chars)

             _                  (prn repeat-start)
             _                  (prn repeat-end)

             repeat-str         (subs data repeat-start #_(min repeat-start (count data)) repeat-end #_(min repeat-end (count data)))

             _                  (prn repeat-str)
             included-groups    (count (u/re-pos #"\((\d+)x(\d+)\)" repeat-str))

             _                  (prn included-groups)
             next-group-nth     (+ 1 current-index included-groups)

             next-group-i       (try
                                  (if-let [[next-g-idx] (nth groups next-group-nth)]
                                    next-g-idx
                                    (count data))
                                  (catch Exception e
                                    (count data)))
;  19473 too low
             _                  (prn next-group-i)

             next-str           (subs data repeat-end #_(min repeat-end (count data)) (min next-group-i (count data)))]
         (recur (str
                 res
                 (subs data current-index #_(min current-index (count data)) idx-group #_(min idx-group (dec (count data))))
                 (str/join "" (repeat n-repeat repeat-str))
                 next-str)
                next-group-i
                next-group-nth))

       
       (str res (subs data (min current-index (dec (count data))) (dec (count data))))))
   (str/replace #" " "")
   count)
)

;; ## Part 1
(defn part-1
  [data]
  data
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
(deftest test-2016-09
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results