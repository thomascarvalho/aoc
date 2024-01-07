^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.23
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "23" "2015"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map (fn [l]
              (u/instaparse l
                            "S = cmd <' '> (target | count) (<', '> count)?
                            cmd = #'\\w+'
                            target = #'\\w'
                            move = #'.*'
                            count = #'[\\+-]?\\d+'
                            "
                            {:cmd    #(->> % keyword (hash-map :cmd))
                             :target #(->> %  (hash-map :target))
                             :count  #(->> % parse-long (hash-map :count))
                             :S      merge})))))

(def input (->> (slurp (io/resource "inputs/2015/23.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "inc a
jio a, +2
tpl a
inc a"))

;; hlf r sets register r to half its current value, then continues with the next instruction.
;; tpl r sets register r to triple its current value, then continues with the next instruction.
;; inc r increments register r, adding 1 to it, then continues with the next instruction.
;; jmp offset is a jump; it continues with the instruction offset away relative to itself.
;; jie r, offset is like jmp, but only jumps if register r is even ("jump if even") .
;; jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd) .


;; ## Part 1
(defn part-1
  [instructions]

  (->
   (loop [current-index 0
          value         {"a" 0
                         "b" 0}]
     (let [instr (try (nth instructions current-index)
                      (catch Exception e nil))]
       (if instr
         (let [{:keys [cmd target count]} instr
               v                          (get value target)
               new-v                      (case cmd
                                            :hlf  (/ v 2)
                                            :tpl  (* v 3)
                                            :inc  (inc v)
                                            v)
               new-index                  (case cmd
                                            :jio (if (= v 1)
                                                   (+ current-index count)
                                                   (inc current-index))
                                            :jmp (+ current-index count)
                                            :jie (if (even? v)
                                                   (+ current-index count)
                                                   (inc current-index))
                                            (inc current-index))]
           (recur new-index
                  (assoc value target new-v)))

         value)))
   (get "b")))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [instructions]

  (->
   (loop [current-index 0
          value         {"a" 1
                         "b" 0}]
     (let [instr (try (nth instructions current-index)
                      (catch Exception e nil))]
       (if instr
         (let [{:keys [cmd target count]} instr
               v                          (get value target)
               new-v                      (case cmd
                                            :hlf  (/ v 2)
                                            :tpl  (* v 3)
                                            :inc  (inc v)
                                            v)
               new-index                  (case cmd
                                            :jio (if (= v 1)
                                                   (+ current-index count)
                                                   (inc current-index))
                                            :jmp (+ current-index count)
                                            :jie (if (even? v)
                                                   (+ current-index count)
                                                   (inc current-index))
                                            (inc current-index))]
           (recur new-index
                  (assoc value target new-v)))

         value)))
   (get "b")))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-23
  (testing "part one"
    (is (= 184 (part-1 input))))

  (testing "part two"
    (is (= 231 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results