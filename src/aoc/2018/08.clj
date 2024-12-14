^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.08
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "08" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/parse-out-longs
       (into [])))

(def input (->> (slurp (io/resource "inputs/2018/08.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))

;; ## Part 1
(defn part-1
  [data]
  data)


(defn decode-node [queue]
  (let [[child-nodes-qty metadata-qty & data] queue]
    {:metadata (take-last metadata-qty data)
     :children (if (pos? child-nodes-qty)
                 (loop [current-queue (drop-last metadata-qty data)
                        qty child-nodes-qty]))}))
                   

#_(when-let [node (first queue)]
    (let [[child-nodes-qty metadata-qty & data] node]
      {:metadata (take-last metadata-qty data)
       :children {}}))


;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
#_(deftest test-2018-08
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

