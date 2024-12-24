(ns aoc.2024.24
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [input]
  (let [[data instructions] (str/split input #"\n\n")
        data (str/split data #"\n")
        data (reduce
              (fn [data l]
                (let [[k v] (str/split l #": ")]
                  (assoc data k (parse-long v))))
              {}
              data)
        instructions (str/split instructions #"\n")
        instructions (reduce
                      (fn [arr l]
                        (let [[instr result] (str/split l #" -> ")
                              [n1 operation n2] (str/split instr #" ")]
                          (conj arr {:n1 n1
                                     :operation operation
                                     :n2 n2
                                     :target result})))
                      []
                      instructions)]


    {:data data
     :instructions instructions}))


;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/24.txt"))
                parser))

(def input-example (parser "x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02"))

(def input-example-2 (parser "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj"))

;; Logic

(defn exec [v1 operation v2]
  (case operation
    "AND" (if (or (zero? v1) (zero? v2)) 0 1)
    "OR" (if (or (= v1 1) (= v2 1)) 1 0)
    "XOR" (if (= v1 v2) 0 1)))

;; ## Part 1
(defn solve-1
  [{:keys [data instructions]}]
  (loop [data data
         [{:keys [n1 operation n2 target] :as instruction} & instructions] instructions
         step 0]
    (if (or (not instruction) (> step 10000))
      data
      (let [v1 (get data n1)
            v2 (get data n2)]
        (if (and (not (nil? v1))
                 (not (nil? v2)))
          (let [result (exec v1 operation v2)]
            (do
              #_(println (count instruction))
              (recur
               (assoc data target result)
               instructions
               (inc step))))
          (recur
           data
           (concat instructions [instruction]) #_(conj instructions instruction)
           (inc step)))))))

(defn part-1 [data]
  (let [data (solve-1 data)]
    (Long/parseLong (->> (into (sorted-map) data)
                         (filter (fn [[k]]
                                   (zero? (.indexOf k "z"))))
                         (mapv second)
                         reverse
                         (reduce str)) 2)))


(part-1 input)

;; ## Part 2
#_(defn part-2
    [data]
    data)

;; # Tests
#_{:nextjournal.clerk/visibility {:code   :show
                                  :result :hide}}
#_(deftest test-2024-24
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

