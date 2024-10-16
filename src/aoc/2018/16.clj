^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.16
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "16" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (let [[samples' prog] (str/split data #"\n\n\n")]
    {:samples (->> (str/split samples' #"\n\n")
                   (mapv (fn [s]
                           (let [[b i a] (mapv #(->> % u/parse-out-longs (into [])) (str/split s #"\n"))]
                             {:before b
                              :instruction i
                              :after a}))))
     :program prog}))



(def input (->> (slurp (io/resource "inputs/2018/16.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]"))

(defn process-instruction [{:keys [type instruction registers] :as m}]
  (let [[op iA iB iC] instruction
        [rA rB rC rD] registers
        reg (fn [v] (get registers v))]

    (case type
      :addr (let [r (+ (reg iA) (reg iB))]
              (assoc-in m [:registers 2] r))
      :addi (let [r (+ (reg iA) iB)]
              (assoc-in m [:registers 2] r))
      :mulr (let [r (* (reg iA) (reg iB))]
              (assoc-in m [:registers 2] r))
      :muli (let [r (* (reg iA) iB)]
              (assoc-in m [:registers 2] r))
      :banr (let [r (bit-and (reg iA) (reg iB))]
                 (assoc-in m [:registers 2] r))
      :bani (let [r (bit-and (reg iA) iB)]
                 (assoc-in m [:registers 2] r))
      :borr (let [r (bit-or (reg iA) (reg iB))]
                 (assoc-in m [:registers 2] r))
      :bori (let [r (bit-or (reg iA) iB)]
                 (assoc-in m [:registers 2] r))

      :setr (assoc-in m [:registers 2] (reg iA))
      :seti (assoc-in m [:registers 2] iA)

      :gtir (assoc-in m [:registers 2] (if (> iA (reg iB)) 1 0))
      :gtri (assoc-in m [:registers 2] (if (> (reg iA) iB) 1 0))
      :gtrr (assoc-in m [:registers 2] (if (> (reg iA) (reg iB)) 1 0))

      :eqir (assoc-in m [:registers 2] (if (= iA (reg iB)) 1 0))
      :eqri (assoc-in m [:registers 2] (if (= (reg iA) iB) 1 0))
      :eqrr (assoc-in m [:registers 2] (if (= (reg iA) (reg iB)) 1 0))



      m)))

(defn process-sample [{:keys [before instruction after] :as sample}]
  (->> [:addi :addr :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr :eqir :eqri :eqrr]
       (reduce (fn [s instr-type]
                 (if (= (->> (assoc s :type instr-type)
                             (process-instruction)
                             :registers)
                        after)
                   (update s :matches conj instr-type)
                   s))
               (-> sample
                   (assoc :registers before)
                   (dissoc :before)))))



(let [{:keys [samples]} input]
  (->> samples 
       (map process-sample)
       ; last
       (filter (fn [{:keys [matches]}]
                 (>= (count matches) 3)))
       count))

;; ## Part 1
(defn part-1
  [data]
  data)


;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2018-16
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/test-render #'test-2018-16)
