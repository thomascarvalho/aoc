(ns aoc.2024.17
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.math :as math]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (let [[A B C & program] (->> data u/parse-out-longs)]
    {:registers {:A A :B B :C C}
     :program (into [] program)}))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/17.txt"))
                parser))

(def input-example (parser "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"))

;; Logic

(defn combo-> [operand {:keys [A B C] :as _registers}]
  (case operand
    (0 1 2 3) operand
    4 A
    5 B
    6 C
    nil))

(defn instruction [opcode operand {:keys [A B C] :as registers}]
  (let [combo-operand (combo-> operand registers)]
    (case opcode
      0 {:registers (assoc registers :A (biginteger (/ A (math/pow 2 combo-operand))))}
      1 {:registers (assoc registers :B (.xor (biginteger B) (biginteger operand)))}
      2 {:registers (assoc registers :B (mod combo-operand 8))}
      3 (cond-> {:registers registers}
          (not (zero? A)) (assoc :jump operand))
      4 {:registers (assoc registers :B (.xor (biginteger B) (biginteger C)))}
      5 {:registers registers
         :output (mod combo-operand 8)}
      6 {:registers (assoc registers :B (biginteger (/ A (math/pow 2 combo-operand))))}
      7 {:registers (assoc registers :C (biginteger (/ A (math/pow 2 combo-operand))))})))

(defn init-registers [{:keys [A B C] :as registers}]
  (cond-> registers
    (not A) (assoc :A 0)
    (not B) (assoc :B 0)
    (not C) (assoc :C 0)))

(defn run [{:keys [registers program]}]
  (loop [pointer 0
         registers (init-registers registers)
         outputs []]
    (let [[opcode operand] [(get program pointer) (get program (inc pointer))]]
      (if (and opcode operand)
        (let [{:keys [registers jump output]} (instruction opcode operand registers)]
          (if (number? jump)
            (recur
             jump
             registers
             outputs)
            (recur
             (+ pointer 2)
             registers
             (if output
               (conj outputs output)
               outputs))))
        {:outputs (mapv int outputs)
         :registers registers}))))

;; ## Part 1
(defn part-1
  [data]
  (->> (run data)
       :outputs
       (str/join ",")))

;; ## Part 2
(defn part-2
  [{:keys [program] :as data}]
  (let [p (mapv #(biginteger %) program)]
    (loop [a (biginteger 1)]
      (let [r (:outputs (run (assoc-in data [:registers :A] a)))
            xs (into [] (drop (- (count p) (count r)) program))]
        (cond
          (= p r) a
          (= r xs) (recur (.multiply a (biginteger 8)))
          :else (recur (.add a (biginteger 1))))))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-17
  (testing "logic"
    (is (= (:registers (run {:registers {:C 9}
                             :program [2 6]}))
           {:A 0 :B 1 :C 9}))
    (is (= (:outputs (run {:registers {:A 10}
                           :program [5 0 5 1 5 4]}))
           [0 1 2]))
    (is (= (run {:registers {:A 2024}
                 :program [0 1 5 4 3 0]})
           {:outputs [4 2 5 6 7 7 7 7 3 1 0]
            :registers {:A 0 :B 0 :C 0}}))

    (is (= (:registers (run {:registers {:B 29}
                             :program [1 7]}))
           {:A 0 :B 26 :C 0}))

    (is (= (:registers (run {:registers {:B 2024 :C 43690}
                             :program [4 0]}))
           {:A 0 :B 44354 :C 43690})))
  (testing "part one"
    (is (= "6,7,5,2,1,3,5,1,7" (part-1 input))))

  (testing "part two"
    (is (= 216549846240877 (part-2 input)))))

