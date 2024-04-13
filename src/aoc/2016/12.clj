^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2016.12
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "12" "2016"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn ->keyword-or-number [c]
  (when c
    (if (re-matches #"[a-z]" c)
      (keyword c)
      (parse-long c))))


(defn parser [data]
  (->> data
       u/to-lines
       (map #(let [[c x y] (str/split % #" ")]
               [(keyword c) (->keyword-or-number x) (->keyword-or-number y)]))))

(def input (->> (slurp (io/resource "inputs/2016/12.txt")) ;; Load the resource
                parser))                             ;; Split into lines

;;  Example
(def input-example (parser "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a"))

(defn process-commands [commands initial-values]
  (let [max-idx (dec (count commands))]


    (->
     (loop [idx    0
            values initial-values]
       (if (> idx max-idx)
         values
         (let [[c t1 t2] (nth commands idx)]
           (case c
             :cpy (recur
                   (inc idx)
                   (if (keyword? t1)
                     (assoc values t2 (get values t1 0))
                     (assoc values t2 t1)))

             :inc (recur
                   (inc idx)
                   (update values t1 inc))

             :dec (recur
                   (inc idx)
                   (update values t1 dec))

             :jnz (if (zero? (get values t1 t1))
                    (recur
                     (inc idx)
                     values)
                    (recur
                     (+ idx t2)
                     values))))))
     :a)))

;; ## Part 1
(defn part-1
  [commands]
  (process-commands commands {:a 0
                              :b 0
                              :c 0
                              :d 0}))

;; ## Part 2
(defn part-2
  [commands]
  (process-commands commands {:a 0
                              :b 0
                              :c 1
                              :d 0}))

;; ## Suite
(deftest test-2016-12
  (testing "part one"
    (is (= 318003 (part-1 input))))

  (testing "part two"
    (is (= 9227657 (part-2 input)))))
