^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2016.04
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "04" "2016"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map #(let [r                    (re-seq #"\w+" %)
                   [sector-id checksum] (take-last 2 r)
                   words                (drop-last 2 r)]
               {:words     words
                :sector-id (parse-long sector-id)
                :checksum  (vec checksum)}))))

(def input (->> (slurp (io/resource "inputs/2016/04.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]"))


;; ## Part 1
(defn part-1
  [data]
  (->>
   (for [d     data
         :let  [s (str/join (:words d))
                freqs (->>
                       (frequencies s)
                       (into [])
                       (sort #(let [c1 (compare (second %2) (second %1))]
                                (if (zero? c1)
                                  (compare (first %1) (first %2))
                                  c1))))
                valid? (->>
                        (for [[i c] (map-indexed vector (:checksum d))
                              :let  [[t] (nth freqs i)]]
                          (= c t))
                        (every? true?))]
         :when valid?]
     (:sector-id d))
   (apply +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

(def alphabet (vec "abcdefghijklmnopqrstuvwxyz"))

(defn rotate [c steps]
  (nth (cycle alphabet) (+ (.indexOf alphabet c) steps)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (->>
   (for [{:keys [words sector-id]} data
         :let                      [sentence (->>
                                              (->> (map vec words)
                                                   (map (fn [word]
                                                          (reduce (fn [r c]
                                                                    (str r (rotate c sector-id))) "" word))))
                                              (reduce (fn [r c]
                                                        (str r c " "))
                                                      "")
                                              str/trim)]
         :when                     (= sentence "northpole object storage")]
     sector-id)
   first))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2016-04
  (testing "part one"
    (is (= 278221 (part-1 input))))

  (testing "part two"
    (is (= 267 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results