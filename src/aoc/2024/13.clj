(ns aoc.2024.13
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (->>
   (str/split data #"\n\n")
   (map (fn [line]
          (partition 2 (u/parse-out-longs line))))))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/13.txt"))
                parser))

(def input-example (parser "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"))

;; Logic
(defn bezout-coefficients
  "Calcule les coefficients de Bézout x, y tels que ax + by = gcd(a,b)
   Retourne [x y]"
  [a b]
  (loop [r0 a, r1 b
         s0 1, s1 0
         t0 0, t1 1]
    (if (zero? r1)
      [s0 t0]
      (let [q (quot r0 r1)]
        (recur r1
               (- r0 (* q r1))
               s1
               (- s0 (* q s1))
               t1
               (- t0 (* q t1)))))))

(defn pushs-in-range
  "Trouve une solution où x et y sont dans [0,100]"
  [a b target]
  (let [gcd (math/gcd a b)
        [x y] (bezout-coefficients a b)]
    (when (zero? (mod target gcd))
      (let [factor (/ target gcd)
            x0 (* x factor)
            y0 (* y factor)
            k-step (/ b gcd)
            x-step (/ a gcd)]
        (->> (range -20000 20000)
             (reduce (fn [_ k]
                       (let [x (- x0 (* k k-step))
                             y (+ y0 (* k x-step))]
                         (when (and (< 0 x 100)
                                    (< 0 y 100))
                           (reduced [x y]))))))))))

;; ## Part 1
(defn part-1
  [data]
  (->> (for [[[a-x a-y] [b-x b-y] [target-x target-y]] data
             :let [pushs-x (pushs-in-range a-x b-x target-x)
                   pushs-y (pushs-in-range a-y b-y target-y)]
             :when (and pushs-x (= pushs-x pushs-y))
             :let [[a b] pushs-x]]
         (+ (* a 3) b))
       (reduce + 0)))

;; ## Part 2
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(part-1 input)

#_(deftest test-2024-13
    (testing "part one"
      (is (= 480 (part-1 input-example)))
      (is (= 1 (part-1 input))))

    ;; to low 16515

    #_(testing "part two"
        (is (= 1 (part-2 input)))))



