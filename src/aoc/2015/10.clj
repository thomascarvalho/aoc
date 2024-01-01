^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.10
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u] 
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "10" "2015"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map #(str/trim %))))

(def input (->> (slurp (io/resource "inputs/2015/10.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "1"))

;; ## Part 1
(defn part-1
  [data]
  (->>
   (loop [l    (first data)
          step 0]
     (if (= step 40)
       l
       (recur
        (->> l
             (re-seq #"(.)\1{0,}")
             (map (fn [[s]]
                    (reduce-kv (fn [t k v]
                                 (str t v k)) "" (frequencies s))))
             (apply str))
        (inc step))))
   count))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  (->>
   (loop [l    (first data)
          step 0]
     (if (= step 50)
       l
       (recur
        (->> l
             (re-seq #"(.)\1{0,}")
             (map (fn [[s]]
                    (reduce-kv (fn [t k v]
                                 (str t v k)) "" (frequencies s))))
             (apply str))
        (inc step))))
   count))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-10
  (testing "part one"
    (is (= 329356 (part-1 input))))

  (testing "part two"
    (is (= 4666278 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
