^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.16
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "16" "2021"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines))

(def input (->> (slurp (io/resource "inputs/2021/16.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser ""))

(def hexa-char->bit-vec
  {\0 [0 0 0 0]
   \1 [0 0 0 1]
   \2 [0 0 1 0]
   \3 [0 0 1 1]
   \4 [0 1 0 0]
   \5 [0 1 0 1]
   \6 [0 1 1 0]
   \7 [0 1 1 1]
   \8 [1 0 0 0]
   \9 [1 0 0 1]
   \A [1 0 1 0]
   \B [1 0 1 1]
   \C [1 1 0 0]
   \D [1 1 0 1]
   \E [1 1 1 0]
   \F [1 1 1 1]})

(def three-bits->hexa
  {[0 0 0] 0
   [0 0 1] 1
   [0 1 0] 2
   [0 1 1] 3
   [1 0 0] 4
   [1 0 1] 5
   [1 1 0] 6
   [1 1 1] 7})

(defn join-to-str [a]
  (str/join "" a))

(defn get-version [bits]
  (->> bits
       (take 3)
       three-bits->hexa))

(defn get-type-id [bits]
  (->> bits
       (drop 3)
       (take 3)
       three-bits->hexa))

(defn get-length-type-id [bits]
  (->> bits
       (drop 6)
       first))

(defn decode-hexa-to-bits [s]
  (->> s
       char-array
       (map hexa-char->bit-vec)
       (apply concat))

  ;
  )

(defn binary->decimal [b]
  (Long/parseLong (if (vector? b) (join-to-str b) b) 2))

(defn get-literal-value-decimal [all-bits]
  (let [version           (get-version all-bits)
        truncated-bits    (drop 6 all-bits)
        final-bits-groups (loop [grouped-bits (partition 5 truncated-bits)
                                 final-bits   []]
                            (if (seq grouped-bits)
                              (let [[start & bits] (first grouped-bits)]
                                (if (= start 1)
                                  (recur (next grouped-bits) (conj final-bits bits))
                                  (conj final-bits bits)))
                              final-bits))
        final-bits        (->> final-bits-groups
                               (apply concat)
                               (into []))]
    [{:version version
      :value   (binary->decimal final-bits)} {:rest (into [] (drop (+ (count final-bits-groups) (count final-bits)) truncated-bits))}]))


(defn  decode-15-bits-operator [bits decode-packet]
  (let [sub-packets-length (->> bits
                                (drop 7)
                                (take 15)
                                (into [])
                                binary->decimal)]
    (decode-packet (->> bits
                        (drop (+ 7 15))
                        (take sub-packets-length)))))

(defn  get-15-bits-operator-subpacket-bits [bits]
  (let [sub-packets-length (->> bits
                                (drop 7)
                                (take 15)
                                (into [])
                                binary->decimal)]
    [(->> bits
          (drop (+ 7 15))
          (take sub-packets-length)
          (into [])) {:rest (drop (+ 7 15 sub-packets-length) bits)}]))

(defn  get-11-bits-operator-subpacket-bits [bits]
  (let [sub-packets-count (->> bits
                               (drop 7)
                               (take 11)
                               (into [])
                               binary->decimal)]
    [(->> bits
          (drop (+ 7 11))
          (take (* 11 sub-packets-count))
          (into [])) {:rest (drop (+ 7 11 (* 11 sub-packets-count)) bits)}]))

(defn get-operator [bits]
  (let [length-type-id                   (get-length-type-id bits)
        version                          (get-version bits)
        [subpackets-bits {:keys [rest]}] (case length-type-id
                                           0 (get-15-bits-operator-subpacket-bits bits)
                                           1 (get-11-bits-operator-subpacket-bits bits))]
    [{:version         version
      :length-type-id  length-type-id
      :subpackets-bits subpackets-bits} {:rest rest}]))


;; ## Part 1
(defn part-1
  [data]
  data
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input]

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)

(defn decode-level-1 [initial-bits]
  (loop [prev-bits initial-bits
         bits    initial-bits
         packets []]
  
    (if (> (count bits) 7)
      (let [type-id                 (get-type-id bits)
            [packet {:keys [rest]}] (if (= 4 type-id)
                                      (get-literal-value-decimal bits)
                                      (get-operator bits))]

        (recur bits rest (conj packets packet)))
      packets)))


(defn decode-level-2 [level-1-packets]

  (mapv (fn [packet]
          (case (:length-type-id packet)
            0 (assoc packet :subpackets (decode-level-1 (:subpackets-bits packet)))
            1 (assoc packet :subpackets (decode-level-1 (:subpackets-bits packet)))

            packet))
        level-1-packets))


(defn decode [hexa]
  (->> hexa
       decode-hexa-to-bits
       decode-level-1
       #_decode-level-2))


;; Tests
(deftest test-2021-16
  (testing "decode to bits"
    (is (= [1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0] (decode-hexa-to-bits "D2FE28")))
    (is (= "110100101111111000101000" (join-to-str (decode-hexa-to-bits "D2FE28")))))

  (testing "decode packet"
    ;; literal 
    (is (= 6 (get-version [1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0])))
    (is (= 4 (get-type-id [1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0])))

    #_(is (= [{:version 6
             :value   2021}] (decode-level-1 [1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0])))

    ;; operators 
    #_(is (= [{:version         1
             :length-type-id  0
             :subpackets-bits [1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0]}] (decode-level-1 (decode-hexa-to-bits "38006F45291200")))) ;; length type id 0

    #_(is (= [{:version         1
             :length-type-id  0
             :subpackets-bits [1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0]
             :subpackets      [{:version 6
                                :value   10}
                               {:version 2
                                :value   20}]}] (decode-level-2 [{:version         1
                                                                  :length-type-id  0
                                                                  :subpackets-bits [1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0]}])))

    ;; full decode 
    #_(is (= [{:version         1
             :length-type-id  0
             :subpackets-bits [1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0]
             :subpackets      [{:version 6
                                :value   10}
                               {:version 2
                                :value   20}]}] (decode "38006F45291200")))

    #_(is (= [{:version         7
             :length-type-id  1
             :subpackets-bits [0 1 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1]
             :subpackets      [{:version 2
                                :value   1}
                               {:version 4
                                :value   2}
                               {:version 1
                                :value   3}]}] (decode "EE00D40C823060")))

    #_(is (= "" (decode "8A004A801A8002F478")))

    #_(is (= "" (decode "620080001611562C8802118E34")))
  


    ;
    )
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

#_(part-1 input-example)