(ns aoc.2022.02
  (:require [util :refer [read-from-ns]]
            [clojure.test :refer :all]
            [clojure.string :as cs]
            [instaparse.core :as insta]))

(def input (read-from-ns ::x))

(def SHAPES-POINTS {:R 1
                    :P 2
                    :S 3})

(def POINTS {:R {:R 3
                 :P 0
                 :S 6}
             :P {:R 6
                 :P 3
                 :S 0}
             :S {:R 0
                 :P 6
                 :S 3}})

(defn parser [i]
  (->> i
       ((insta/parser
         "<RESULT> = A<space>B
     <A> = R | P | S
     <B> = R | P | S
     R = 'A' | 'X'
     P = 'B' | 'Y'
     S = 'C' | 'Z'
     space = ' '"))
       (insta/transform {:R (fn [x] :R)
                         :S (fn [x] :S)
                         :P (fn [x] :P)})))

(defn part-one []
  (let [rounds (map parser (cs/split-lines input))]
    (->>
     (map (fn [[opponent me]]
            (let [shape-points (get SHAPES-POINTS me)
                  points       (get-in POINTS [me opponent])]
              (+ shape-points points))) rounds)
     (reduce + 0))))


(defn find-piece
  [opponent win-status]
  (case win-status
    ;; Lose
    :R (case opponent
         :R :S
         :S :P
         :P :R)

    ;; Draw
    :P opponent

    ;; Win
    :S (case opponent
         :S :R
         :P :S
         :R :P)))

(defn part-two []
  (let [rounds (map parser (cs/split-lines input))]
    (->>
     (map (fn [[opponent win-status]]
            (let [piece        (find-piece opponent win-status)
                  shape-points (get SHAPES-POINTS piece)
                  points       (get-in POINTS [piece opponent])]
              (+ shape-points points))) rounds)
     (reduce + 0))))

(deftest test-2022-02
  (testing "part one"
    (is (= 11150 (part-one))))

  (testing "part two"
    (is (= 8295 (part-two)))))