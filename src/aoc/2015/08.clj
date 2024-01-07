^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.08
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "08" "2015"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines))

(def input (->> (slurp (io/resource "inputs/2015/08.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (->> (slurp (io/resource "inputs/2015/08-example.txt")) ;; Load the resource
                        parser))

(defn nb-code [l]
  (count l))

(defn nb-memory [l]
  (let [s1 (str/replace l #"\\\\" "Z")
        s2 (str/replace (subs s1 1 (dec (count s1))) #"\\\"" "Z")
        s3 (str/replace s2 #"\\x\w{2}" "D")
        r  (read-string (str "\"" s3 "\""))]
    (count r)))

;; ## Part 1
(defn part-1
  [lines]
  (let [r (mapv #(vector (nb-code %) (nb-memory %)) lines)]
    (- (reduce + (map first r))
       (reduce + (map second r)))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn nb-encoded [l]
  (let [r (str "\"" (str/escape l {\\ "\\\\"
                                   \" "\\\""}) "\"")]
    (count r)))

(defn part-2
  [lines]
  (let [r       (mapv #(vector (nb-encoded %) (nb-code %)) lines)
        encoded (reduce + (map first r))
        coded   (reduce + (map second r))]
    (- encoded coded)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-08
  (testing "part one"
    (is (= 1333 (part-1 input))))

  (testing "part two"
    (is (= 2046 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results