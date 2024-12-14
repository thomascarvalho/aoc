^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.07
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [util :as u]
            [clojure.set :refer [union]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

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
                            VALUE = (INT | WYRE)
                            PRED = (WYRE | INT) <' '> EVAL <' '> (WYRE | INT)
                            EVAL = #'[A-Z]+'
                            INT = #'\\d+'
                            TO = WYRE
                            WYRE = CHARS
                            NOT = <'NOT '> WYRE
                            <CHARS> = #'[a-z]+'
                            "
                           {:INT  parse-long
                            #_#_#_#_#_#_:S    (fn [d _ #_#_from to]

                                                d
                                                #_{:from from
                                                   :to to} #_(cons to from))
                                    :COND (fn [[t a b]]
                                            {:op t}
                                            #_[t
                                               (cond-> [a]
                                                 b (conj b))])
                                :PRED (fn [a ev b]
                                        {:op ev #_[ev a b]})
                            :S (fn [& data]
                                 (hash-map (:to (second data)) (merge (second data) (first data))))
                            :COND (fn [[a b c d]]
                                    (cond-> {}
                                      (= a :VALUE) (assoc :value b)
                                      (= a :PRED) (assoc :left b :op c :right d :deps (filterv keyword? [b d]))
                                      (= a :NOT) (assoc :left b :op a :deps [b])))


                            :TO (fn [k]
                                  {:to k})
                            :EVAL keyword
                            :WYRE keyword}))
       (apply merge)))

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


(let [data input-example]
  (loop [queue [:g]
         path  #{}]
    (let [[current & next-queue] queue
          nexts                  (get-parents data path current)]
      (if current
        (recur (concat next-queue nexts) (conj path current))
        path))))

(defn get-v [m k]
  (if (keyword? k)
    (get m k)
    k))

;; ## Part 1
(defn part-1
  [data]
  (let [edges (->> data
                   vals
                   (reduce (fn [edges {:keys [to value deps]}]
                             (cond
                               (keyword? value) (concat edges [[value to]])

                               (not value) (concat edges (mapv (fn [dep]
                                                                 [dep to]) deps))
                               :else edges))
                           []))
        g (uber/graph)
        g-with-edges (-> g
                         (uber/add-directed-edges* edges))

        sorted-dests (alg/topsort g-with-edges)]
    (->> sorted-dests
         (reduce (fn [m to]
                   (let [{:keys [value op left right]} (get data to)]
                     (assoc m to
                            (cond
                              value (get-v m value)
                              op (case op
                                   :AND (bit-and (get-v m left) (get-v m right))
                                   :OR (bit-or (get-v m left) (get-v m right))
                                   :LSHIFT (bit-shift-left (get-v m left) (get-v m right))
                                   :RSHIFT (bit-shift-right (get-v m left) (get-v m right))
                                   :NOT (inc (+ 65535 (bit-not (get-v m left))))
                                   :WYRE-LINK (get-v m left)))))) {})
         :a)))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [new-data (assoc data :b {:to :b :value (part-1 data)})
        edges (->> new-data
                   vals
                   (reduce (fn [edges {:keys [to value deps]}]
                             (cond
                               (keyword? value) (concat edges [[value to]])

                               (not value) (concat edges (mapv (fn [dep]
                                                                 [dep to]) deps))
                               :else edges))
                           []))
        g (uber/graph)
        g-with-edges (-> g
                         (uber/add-directed-edges* edges))

        sorted-dests (alg/topsort g-with-edges)]
    (->> sorted-dests
         (reduce (fn [m to]
                   (let [{:keys [value op left right]} (get new-data to)]
                     (assoc m to
                            (cond
                              value (get-v m value)
                              op (case op
                                   :AND (bit-and (get-v m left) (get-v m right))
                                   :OR (bit-or (get-v m left) (get-v m right))
                                   :LSHIFT (bit-shift-left (get-v m left) (get-v m right))
                                   :RSHIFT (bit-shift-right (get-v m left) (get-v m right))
                                   :NOT (inc (+ 65535 (bit-not (get-v m left))))
                                   :WYRE-LINK (get-v m left)))))) {})
         :a)))


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-07
  (testing "part one"
    (is (= 46065 (part-1 input))))

  (testing "part two"
    (is (= 14134 (part-2 input)))))


