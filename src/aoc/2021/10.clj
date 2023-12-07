^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2021.10
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int instaparse]]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [instaparse.core :as insta]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "10" "2021"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(def instaparser #(instaparse % "S = group*
                                      group = (parents | brackets | mustaches | angle-brackets)
                                      subgroup = (parents | brackets | mustaches | angle-brackets)*
                                      mismatch = (openers | closers)
                                      openers = '(' | '{' | '[' | '<'
                                      closers = ')' | '}' | ']' | '>'
                                      parents = '(' (subgroup? | mismatch?) ')'?
                                      brackets = '[' (subgroup? | mismatch?) ']'?
                                      mustaches = '{' (subgroup? | mismatch?) '}'?
                                      angle-brackets = '<' (subgroup? | mismatch?) '>'?
                                      " {}))

(defn decode [line]
  (re-seq #"((\((.*)?\))|(\[(.*)?\]))" line)
  )

(defn parser [data]
  (->> data
       str/split-lines
       (map decode)))

(def input (->> (slurp (io/resource "inputs/2021/10.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"))

(defn lint [line]
  (let [freqs (frequencies line)]
    [line {"<>" (- (or (get freqs \<) 0) (or (get freqs \>) 0))
           "{}" (- (or (get freqs \{) 0) (or (get freqs \}) 0))
           "[]" (- (or (get freqs \[) 0) (or (get freqs \]) 0))}]))

(def opener? #{\< \{ \[ \(})

(defn get-opener-from-closer [c]
  (case c
    \} \{
    \> \<
    \] \[
    \) \())

(defn find-first-error [line]


  #_(loop [chars   line
           openers {\< 0
                    \[ 0
                    \{ 0
                    \( 0}
           errors  []]
      (if-not (seq chars)
        {:openers openers
         :errors  errors}
        (let [c (first chars)]
          (if (opener? c)
            (recur (next chars) (update openers c inc) errors)

            (let [opener (get-opener-from-closer c)]
              (if (pos? (get openers opener))
                (recur (next chars) (update openers opener dec) errors)
                (recur (next chars) (update openers opener dec) (conj errors c)))))))))

;; ## Part 1
(defn part-1
  [lines]
  lines
  #_(let [rows (filter #(even? (count %)) lines)]
      (map lint lines))

  #_(find-first-error [\{ \< \[ \[ \] \] \> \} \< \{ \[ \{ \[ \{ \[ \] \{ \( \) \[ \[ \[ \]])

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


;; Tests
(deftest test-2021-10
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

#_(part-1 input-example)