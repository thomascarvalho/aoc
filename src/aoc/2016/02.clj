^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2016.02
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.core.matrix :as ma]
            [util :as u]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html (u/load-problem "02" "2016"))
{:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(def dir->coord {:U [-1 0]
                 :R [0 1]
                 :D [1 0]
                 :L [0 -1]})

(defn parser [data]
  (->> data
       u/to-lines
       (map #(->> %
                  (re-seq #"\w")
                  (map keyword)))))

(def input (->> (slurp (io/resource "inputs/2016/02.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "ULL
RRDDD
LURDL
UUUUD"))

(defn mget [m [y x]]
  (try
    (ma/mget m y x)
    (catch Exception e nil)))

(defn move [m [y x] d]
  (let [[ty tx] (dir->coord d)
        new-x   (+ x tx)
        new-y   (+ y ty)
        new-pos [new-y new-x]]
    (when (mget m new-pos)
      new-pos)))

;; ## Part 1
(defn part-1
  [buttons]
  (let [m (->
           (ma/matrix (range 1 10))
           (ma/reshape [3 3]))]

    (->>
     buttons
     (reduce (fn [[code initial-pos] moves]
               (let [new-pos (reduce (fn [pos d]
                                       (let [new-pos (move m pos d)]
                                         (or new-pos pos))) initial-pos moves)]
                 [(str code (mget m new-pos)) new-pos])) ["" [1 1]])
     first)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [buttons]
  (let [m [[nil nil "1" nil nil]
           [nil "2" "3" "4" nil]
           ["5" "6" "7" "8" "9"]
           [nil "A" "B" "C" nil]
           [nil nil "D" nil nil]]]
    (->>
     buttons
     (reduce (fn [[code initial-pos] moves]
               (let [new-pos (reduce (fn [pos d]
                                       (let [new-pos (move m pos d)]
                                         (or new-pos pos))) initial-pos moves)]
                 [(str code (mget m new-pos)) new-pos])) ["" [2 0]])
     first)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2016-02
  (testing "part one"
    (is (= "92435" (part-1 input))))

  (testing "part two"
    (is (= "C1A88" (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results