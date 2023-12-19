^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.19
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [instaparse.core :as insta]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "19" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn workflow-parser [line]
  (->> line
       ((insta/parser
         "W = workflow <'{'> rules <'}'>
           workflow = #'\\w+'
           rules = (( pred | return) <','>?)*,
           pred = category comparator num <':'> return
           category = #'\\w+'
           comparator = ('<' | '>')
           return = #'\\w+'
           num = #'\\d+'
           "))
       #_{:clj-kondo/ignore [:unresolved-var]}
       (insta/transform {:num        (fn [n]
                                       {:num (parse-long n)})
                         :pred       (fn [& p] (apply merge p))
                         :return     (fn [r]
                                       {:return r})
                         :workflow   str
                         :rules      vector
                         :name       (fn [n]
                                       {:name n})
                         :category   (fn [c]
                                       {:category (keyword c)})
                         :comparator (fn [o] {:comparator (eval (symbol o))})
                         :W          hash-map})))

(defn parser [data]
  (let [[workflows-block ratings-block] (u/to-blocks data)
        workflows                       (->> workflows-block
                                             u/to-lines
                                             (mapv workflow-parser)
                                             (apply merge))
        ratings                         (->> ratings-block
                                             u/to-lines
                                             (mapv (fn [l] (let [[x m a s] (->> l
                                                                                (re-seq #"(\d+)")
                                                                                (map #(-> % second parse-long)))]
                                                             {:x x
                                                              :m m
                                                              :a a
                                                              :s s}))))]

    [workflows ratings]))

(def input (->> (slurp (io/resource "inputs/2023/19.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"))

(defn get-next-workflow-name [wf {:keys [x m a s]}]
  (loop [[{:keys [category comparator num return]} & rules] wf]
    (if category
      (let [n (case category
                :x x
                :m m
                :a a
                :s s)]
        (if (comparator n num)
          return
          (recur rules)))
      return)))

(defn get-path [workflows ratings]
  (loop [path ["in"]]
    (as-> (last path) $
      (get workflows $)
      (get-next-workflow-name $ ratings)
      (case $
        ("A" "R") (conj path $)
        (recur (conj path $))))))

;; ## Part 1
(defn part-1
  [[workflows ratings]]
  (->> ratings
       (reduce
        (fn [t r]
          (let [path (get-path workflows r)]
            (if (= (last path) "A")
              (reduce + t (vals r))
              t))) 0)))

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


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-19
  (testing "part one - example"
    (is (= 19114 (part-1 input-example))))
  
  (testing "part one"
    (is (= 495298 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

#_(part-1 input)