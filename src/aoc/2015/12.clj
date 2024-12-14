^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.12
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.data.json :as json]
            [jsonista.core :as j]
            [ubergraph.core :as uber]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data))

(def input (->> (slurp (io/resource "inputs/2015/12.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))

;; ## Part 1
(defn part-1
  [data]
  (->> data
       u/parse-out-longs
       (reduce +)))
  ;


;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-1 input)

(defn flatten-tree [x]
  (filter (complement map?)
          (rest (tree-seq map? seq x))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  (let [s data #_"{\"e\":-48,\"a\":6,\"d\":-6,\"c\":4,\"h\":\"yellow\",\"b\":\"blue\",\"g\":\"red\",\"f\":\"red\"}"]
    (->> s
         (re-seq #"\{[^{]{0,}\"\w*\":\"red\"[^\}]{0,}\}")
         (reduce (fn [s r]
                   (str/replace s r "")) s)
         u/parse-out-longs
         (reduce +)))
  #_(->>
     (str/replace data #"\{[^{]*\"\w*\":\"red\"[^\}]*\}" "")
     u/parse-out-longs
     (reduce +))

  #_(let [j (json/read-str data)]
      #_(flatten-tree j)
      (vals j)))

  ;


;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
#_(deftest test-2015-12
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
;; 91709, 105394 - too high
; (part-2 input)


