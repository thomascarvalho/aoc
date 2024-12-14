^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2016.05
  {:nextjournal.clerk/toc true}
  (:import java.security.MessageDigest
           java.math.BigInteger)
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution

(defn parser [data]
  (->> data
       u/to-lines
       first))

(def input (->> (slurp (io/resource "inputs/2016/05.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "abc"))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw       (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn brute-force [^String s start]
  (reduce
   (fn [_ n]
     (let [r (md5 (str s n))]
       (when (str/starts-with? r "00000") (reduced [n (nth r 5)]))))
   (range start Double/MAX_VALUE)))

;; ## Part 1
(defn part-1
  [data]
  (->>
   (->> (range 8)
        (reduce
         (fn [[start code] _]
           (let [[e c] (brute-force data start)]
             [(inc e) (str code c)]))
         [1 ""]))
   second))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (let [c (->>
           (->> (range 8)
                (reduce
                 (fn [[start m] _]
                   (let [[e pos v] (reduce
                                    (fn [_ n]
                                      (let [r (md5 (str data n))]
                                        (when (str/starts-with? r "00000")
                                          (let [sixth (str (nth r 5))]
                                            (when (and
                                                   (some #{sixth} ["0" "1" "2" "3" "4" "5" "6" "7"])
                                                   (not (get m sixth)))
                                              (reduced [n sixth (str (nth r 6))]))))))
                                    (range start Double/MAX_VALUE))]
                     [(inc e) (assoc m pos v)]))
                 [1 {}]))
           second)]

    (str (get c "0") (get c "1") (get c "2") (get c "3") (get c "4") (get c "5") (get c "6") (get c "7"))))



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2016-05
  (testing "part one"
    (is (= "d4cd2ee1" (part-1 input))))

  (testing "part two"
    (is (= "f2c730e5" (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
