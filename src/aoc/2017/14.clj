^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.14
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [pathfinding :as pf]
            [util :as u]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [aoc.2017.10 :refer [knot-hash-output]]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (-> data
      (str/replace #"\n" "")))

(def input (->> (slurp (io/resource "inputs/2017/14.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "flqrgnkx"))

(defn unhexify [hex]
  (apply str
         (map
          (fn [[x y]] (char (Integer/parseInt (str x y) 16)))
          (partition 2 hex))))

(defn knot->binary-numbers [k]
  (->> k
       char-array
       (mapcat #(as-> (-> (str "0x" %)
                          read-string
                          (Integer/toString 2)
                          parse-long) $
                  (format "%04d" $)
                  (str/split $ #"")
                  (map parse-long $)))))

;; ## Part 1
(defn part-1
  [data]
  (->
   (->> (range 0 128)
        (mapcat (fn [suffix]
                  (->>
                   (knot-hash-output (str data "-" suffix))
                   knot->binary-numbers)))
        frequencies)
   (get 1)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [m           (->> (range 0 128)
                         (map (fn [suffix]
                                (->>
                                 (knot-hash-output (str data "-" suffix))
                                 knot->binary-numbers))))
        valid-cells (->> (pf/decode-matrix m)
                         :cells
                         (filter (fn [[_ n]]
                                   (= n 1)))
                         (map (fn [[coords]]
                                coords)))
        edges       (mapcat (fn [coords]
                              (let [[y x]     coords
                                    neighbors (filter #(some #{%} valid-cells)
                                                      [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])]
                                (map #(vector coords %) neighbors))) valid-cells)
        g           (-> (uber/graph)
                        (uber/add-nodes* valid-cells)
                        (uber/add-undirected-edges* edges))]
    (->
     (alg/connected-components g)
     count)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-14
  (testing "part one"
    (is (= 8316 (part-1 input))))

  (testing "part two"
    (is (= 1074 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

