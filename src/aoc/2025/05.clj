(ns aoc.2025.05
  {:nextjournal.clerk/toc true}
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [pathfinding :as pf]
            [clojure.set :as cset]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))


;; # Problem
(def input (->> (slurp (io/resource "inputs/2025/05.txt"))))

(def input-example "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(defn parser [input]
  (let [[ranges ids] (str/split input #"\n\n")]
    (hash-map :ranges (->> ranges
                           str/split-lines
                           (mapv #(let [r (str/split % #"-")]
                                    (mapv biginteger r))))
              :ids (->> ids
                        str/split-lines
                        (mapv biginteger)))))

(defn part-one
  [data]
  (let [{:keys [ids ranges] :as m} (parser data)]
    (reduce
     (fn [t id]
       (if (reduce
            (fn [_ [start end]]
              (if (<= start id end)
                (reduced true)
                false))
            false
            ranges)
         (inc t)
         t))
     0
     ids)))

(defn part-two
  [data]
  (let [{:keys [ids ranges] :as m} (parser data)
        ranges (apply sorted-set ranges)
        r (loop [ranges ranges
                 removed? #{}
                 step 0]
            (if (or (= step 30000) (= 1 (count ranges)))
              ranges
              #_{:ranges ranges
                 :removed? removed?}
              (let [r1 (first ranges)
                    r2 (second ranges)]
                (let [[start-1 end-1] r1
                      [start-2 end-2] r2
                      new-state (cond
                                  (<= start-1 start-2 end-2 end-1) {:result [r1]
                                                                    :removed? #{r2}}
                                  (<= start-2 start-1 end-1 end-2) {:result [r2]
                                                                    :removed? #{r1}}
                                  (<= start-1 start-2 end-1 end-2) {:result [[start-1 end-2]]
                                                                    :removed? #{r1 r2}}
                                  (<= start-2 start-1 end-2 end-1) {:result [[start-2 end-1]]
                                                                    :removed? #{r1 r2}}
                                  :else {:result #{r1 r2}
                                         :removed? #{}})
                      new-ranges   (-> (apply conj ranges (:result new-state))
                                       (cset/difference (:removed? new-state)))

                      new-removed? (apply conj removed? (:removed? new-state))]
                  #_(if (= new-ranges ranges)
                      new-ranges)
                  (recur
                   new-ranges
                   new-removed?
                   (inc step))))))]
    (->> r
         #_(reduce
            (fn [t [start end]]
              (+ t (inc (- end start))))
            0))))

#_(deftest test-2025-05
    (testing "part one"
      (is (= 3 (part-one input-example)))
      (is (= 509 (part-one input))))

    (testing "part two"
      (is (= 43 (part-two input-example)))
      (is (= 8746 (part-two input)))))

(time (part-two input-example))



; 412938283537309N too high

; 400976098864135
