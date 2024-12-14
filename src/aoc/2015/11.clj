^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.11
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       first))

(def input (->> (slurp (io/resource "inputs/2015/11.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "abcdefgh"))
(def all-letters "abcdefghijklmnopqrstuvwxyz")
(def allowed-letters (to-array (-> all-letters
                                   (str/replace #"[iol]" ""))))
(def allowed-partitions
  (as-> all-letters $
    (str/split $ #"[iol]")
    (mapcat #(partition 3 1 %) $)))

(def indexed-letters (map-indexed vector allowed-letters))

(defn get-idx [c]
  (->> indexed-letters
       (reduce (fn [_ [i cc]]
                 (if (= cc c)
                   (reduced i)
                   nil)) nil)))

(defn get-char [i]
  (->> indexed-letters
       (reduce (fn [_ [ii c]]
                 (if (= i ii)
                   (reduced c)
                   nil)) nil)))


(defn inc-char [c]
  (let [i (get-idx c)]
    (get-char (if (= i 22) 0 (inc i)))))

(defn valid? [letters]
  (and
   (every? #(some #{%} allowed-letters) letters) ;; only valid letters
   (reduce (fn [_ p]
             (let [r (some #{p} allowed-partitions)]
               (if r
                 (reduced true)
                 false))) false (partition 3 1 letters)) ;; only allowed partition abc, bcd etc...
   (->>
    letters
    (partition 2 1)
    (filter (fn [[a b]]
              (= a b)))
    set
    count
    (<= 2)))) ;; at least two different, non-overlapping pairs of letters, like aa, bb, or zz
   


;; ## Part 1
(defn part-1
  [data]
  (let [initial-letters (into [] (to-array data))
        last-index      (dec (count initial-letters))]
    (loop [idx     last-index
           letters initial-letters
           step    0]
      (if (or (= step 5000000) (and (not= step 0) (valid? letters)))
        (str/join letters)
        (let [new-char (inc-char (nth letters idx))]
          (recur
           (if (= new-char \a)
             (if (zero? idx)
               last-index
               (dec idx))
             last-index)
           (assoc letters idx new-char)
           (inc step)))))))
  ;
  


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (-> data
      part-1
      part-1))

  ;
  



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-11
  (testing "part one"
    (is (= "hepxxyzz" (part-1 input))))

  (testing "part two"
    (is (= "heqaabcc" (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
