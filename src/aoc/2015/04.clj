^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.04
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

(def input (->> (slurp (io/resource "inputs/2015/04.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw       (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn brute-force [^String s ^String start-string]
  (let [start 1
        end   (parse-long (apply str  "1" (take (-> s count) (repeat 0))))]
    (reduce
     (fn [_ n]
       (let [r (md5 (str s n))]
         (when (str/starts-with? r start-string) (reduced n))))
     (range start end))))

;; ## Part 1
(defn part-1
  [s]
  (brute-force s "00000"))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [s]
  (brute-force s "000000"))



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-04
  (testing "part one"
    (is (= 117946 (part-1 input))))

  (testing "part two"
    (is (= 3938038 (part-2 input)))))
