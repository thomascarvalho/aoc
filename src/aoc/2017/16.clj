^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.16
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [clojure.pprint :as pprint]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (for [cmd  (str/split (str/replace data #"\n" "") #",")
        :let [op (keyword (str (first cmd)))]]
    [op
     (case op
       :s (u/parse-out-longs cmd)
       :x (u/parse-out-longs cmd)
       :p (str/split (subs cmd 1) #"/"))]))

(def input (->> (slurp (io/resource "inputs/2017/16.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "s1,x3/4,pe/b"))

(defn do-the-dance [cmds programs]
  (->> cmds
       (reduce (fn [programs [op [a b]]]
                 (case op
                   :s (let [spins    (take-last a programs)
                            programs (drop-last a programs)]
                        (into [] (concat spins programs)))
                   :x (let [a-val (nth programs a)
                            b-val (nth programs b)]
                        (-> programs
                            (assoc a b-val)
                            (assoc b a-val)))
                   :p (let [a-idx (.indexOf programs a)
                            b-idx (.indexOf programs b)]
                        (-> programs
                            (assoc a-idx b)
                            (assoc b-idx a))))) programs)))

;; ## Part 1
(defn part-1
  [data]
  (let [programs ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"]
        cmds     data]
    (->> (do-the-dance cmds programs)
         (str/join ""))))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(defn part-2
  [data]
  (let [programs                   ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"]
        cmds                       data
        dance                      (partial do-the-dance cmds)

        [progs offset-idx n-cycle] (loop [times         0
                                          programs      programs
                                          last-programs []]
                                     (let [program (dance programs)
                                           idx     (.indexOf last-programs program)]
                                       (if (or (= times 1000) (pos? idx))
                                         [last-programs idx times]
                                         (recur
                                          (inc times)
                                          program
                                          (conj last-programs
                                                program)))))]

    (->> n-cycle
         (mod 1000000000)
         (+ offset-idx)
         (nth progs)
         (str/join ""))))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-16
  (testing "part one"
    (is (= "bkgcdefiholnpmja" (part-1 input))))

  (testing "part two"
    (is (= "knmdfoijcbpghlea" (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

