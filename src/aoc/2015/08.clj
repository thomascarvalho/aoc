^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.08
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "08" "2015"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

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
  (let [r (read-string
           (-> l
               (str/replace #"\"" "D")
               (str/replace #"\\x\w{2}" "D")
               #_(str/replace #"\\\\" "D")))]

    (count (pr-str r))))

;; ## Part 1
(defn part-1
  [lines]
  (let [r (mapv #(vector (nb-code %) (nb-memory %)) lines)]
    #_(map second r)
    (- (reduce + (map first r))
       (reduce + (map second r))))
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
(deftest test-2015-08
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
;; 1220 too low
;; 2588 too high
(part-1 input)