^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.15
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "15" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (-> data
      str/trim-newline
      (str/split #",")))

(def input (->> (slurp (io/resource "inputs/2023/15.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"))

(defn get-hash [s]
  (reduce (fn [current ch]
            (-> ch
                int
                (+ current)
                (* 17)
                (rem 256))) 0 s))

;; ## Part 1
(defn part-1
  [strings]
  (->>
   (map get-hash strings)
   (reduce +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

(defn first-index-for-label [boxes label]
  (->>
   (map-indexed vector boxes)
   (reduce (fn [_ [i [l]]] (when (= label l) (reduced i))) nil)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [strings]
  (->>
   strings
   (reduce
    (fn [all-boxes s]
      (let [[_ label op focal] (re-find #"(\w+)([=-])(\d+)?" s)
            index              (get-hash label)
            current-boxes      (get all-boxes index)
            assoc-box-index    (first-index-for-label current-boxes label)]
        (case op
          "-" (if (and (seq current-boxes) assoc-box-index)
                (assoc all-boxes index (u/vec-remove assoc-box-index current-boxes))
                all-boxes)
          "=" (if (seq current-boxes)
                (if assoc-box-index
                  (assoc-in all-boxes [index assoc-box-index] [label focal])
                  (update all-boxes index conj [label focal]))
                (assoc all-boxes index [[label focal]])))))
    {})
   (reduce-kv
    (fn [total k boxes]
      (if (seq boxes)
        (+ total (reduce (fn [t [i [_label focal]]]
                           (let [r (* (inc k) (inc i) (parse-long focal))]
                             (+ t r))) 0 (map-indexed vector boxes)))
        total))
    0))

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
(deftest test-2023-15
  (testing "part one"
    (is (= 506269 (part-1 input))))

  (testing "part two - example"
    (is (= 145 (part-2 input-example))))

  (testing "part two"
    (is (= 264021 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

