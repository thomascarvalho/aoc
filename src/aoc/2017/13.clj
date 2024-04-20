^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.13
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "13" "2017"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map u/parse-out-longs)))

(def input (->> (slurp (io/resource "inputs/2017/13.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "0: 3
1: 2
4: 4
6: 4"))

;; ## Part 1

(defn assoc-initial-layer [layers [x size]]
  (assoc layers x {:y    0
                   :op   inc
                   :size size}))

(defn toggle-op [op]
  (if (= op inc)
    dec
    inc))

(defn move-layers [layers]
  (->> layers
       (reduce (fn [layers [x {:keys [y op size]
                               :as   layer}]]
                ;; (prn x)
                ;; (prn layer)
                 (->>
                  (let [new-y (op y)]
                    (if (or (< new-y 0) (>= new-y size))
                      (let [new-op (toggle-op op)]
                        (assoc layer :op new-op :y (new-op y)))
                      (assoc layer :y new-y)))
                  (assoc layers x))) {})))

(defn part-1 [data]
  (let [layers (reduce assoc-initial-layer {} data)]
    (->> (range 0 (inc (apply max (keys layers))))
         (reduce (fn [{:keys [layers caught]} x]
                   (prn x)
                   (let [caught? (try
                                   (zero? (get-in layers [x :y]))
                                   (catch Exception e
                                     false))]


                     {:layers (move-layers layers)
                      :caught (cond-> caught
                                caught? (conj [x (get-in layers [x :size])]))}))
                 {:layers layers
                  :caught []})
         :caught
         (map (partial apply *))
         (apply +))))



;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn part-2
  [data]
  (time
   (let [initial-layers (reduce assoc-initial-layer {} data)
         layers-range   (range 0 (inc (apply max (keys initial-layers))))]
     (loop [delay  0
            layers initial-layers]
       (let [caught? (->> layers-range
                          (reduce (fn [layers x]
                                    (let [caught? (try
                                                    (zero? (get-in layers [x :y]))
                                                    (catch Exception e
                                                      false))]
                                      (if caught?
                                        (reduced false)
                                        (move-layers layers))))
                                  layers))]
         (if (false? caught?)
           (recur (inc delay) (move-layers layers))
           delay))))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-13
  (testing "part one"
    (is (= 632 (part-1 input))))

  #_(testing "part two" ;; TODO: optimize, commented because very long
      (is (= 3849742 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

(t/test-render #'test-2017-13)