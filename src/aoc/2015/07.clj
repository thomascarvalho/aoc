^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.set :refer [union]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "07" "2015"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map #(u/instaparse %
                           "S = COND <' -> '> TO
                            COND = (WYRE-LINK | VALUE | PRED | NOT)
                            WYRE-LINK = WYRE
                            VALUE = INT
                            PRED = (WYRE | INT) <' '> EVAL <' '> (WYRE | INT)
                            EVAL = #'[A-Z]+'
                            INT = #'\\d+'
                            <TO> = WYRE
                            WYRE = CHARS
                            NOT = <'NOT '> WYRE
                            <CHARS> = #'[a-z]+'
                            "
                           {:INT  parse-long
                            :S    (fn [from to]
                                    (cons to from))
                            :COND (fn [[t a b]]
                                    [t
                                     (cond-> [a]
                                       b (conj b))])
                            :EVAL keyword
                            :PRED (fn [a ev b]
                                    [ev a b])
                            :WYRE keyword}))))

(def input (->> (slurp (io/resource "inputs/2015/07.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i"))

(defn get-target [data target]
  (->> data
       (filter #(-> % first (= target)))
       first))


(defn get-parents [data path target]
  (when (and target (not (some #{target} path)))
    (let [[_ _ parents] (get-target data target)]
      parents)))


(let [data input]
  (loop [queue [:a]
         path  #{}]
    (let [[current & next-queue] queue
          nexts                  (get-parents data path current)]
      (if current
        (recur (concat next-queue nexts) (conj path current))
        path))))

;; ## Part 1
(defn part-1
  [data]

  #_(loop [current :a]
    (get-parents data #{} current))

  #_(loop [currents [:a]])





data
  #_(reduce (fn [m {:keys [from to]}]
              (let [[t a b] from]
                (assoc m to
                       (case t
                         :VALUE a
                         :AND (bit-and (m a) (m b))
                         :OR (bit-or (m a) (m b))
                         :LSHIFT (bit-shift-left (m a) b)
                         :RSHIFT (bit-shift-right (m a) b)
                         :NOT (inc (+ 65535 (bit-not (m a))))
                         :WYRE-LINK (m a))))) {} (sort-by :sort data))


  ;
  )

(part-1 input-example)

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
(deftest test-2015-07
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
(part-1 input)