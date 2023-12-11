^{:nextjournal.clerk/visibility :hide-ns
  :clerk/no-cache               true}
(ns aoc.2023.10
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [util :as u]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "10" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(def pipe-dirs {\| [:N :S]
                \- [:W :E]
                \7 [:W :S]
                \J [:W :N]
                \L [:E :N]
                \F [:E :S]})

(defn parser [data]
  (->> data
       u/to-matrix))

(def input (->> (slurp (io/resource "inputs/2023/10.txt"))
                parser))
{:nextjournal.clerk/visibility {:result :hide}}

;;  Exa;lmbnn;;nvllllllmmple
(def input-example (parser ".....
.S-7.
.|.|.
.L-J.
....."))

(def directions  [[:E [1 0]]
                  [:S [0 1]]
                  [:W [-1 0]]
                  [:N [0 -1]]])

(defn neighbours [[x y] m]
  (for [[dir [x2 y2]] directions
        :let          [n-coords [(+ x x2) (+ y y2)]
                       target (get m n-coords)]
        :when         (and target (not= target \.))]
    [n-coords target]))

(defn matrix->map [m]
  (->> m
       (map-indexed
        (fn [y row]
          (->> row
               (map-indexed
                (fn [x c]
                  (hash-map [x y] c)))
               (into {}))))
       (into {})))

;; ## Part 1
(defn part-1
  [matrix]

  (let [m     (matrix->map matrix)
        start (first (filter #(= (second %) \S) m))]
    [start (neighbours (first start) m)]
    
    )




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
  nil
  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :hide}}
#_(part-2 input)

;; (defn test-var? [x]
;;   (not (nil? (when-let [v (v/get-safe x :nextjournal.clerk/var-from-def)]
;;                (:test (meta v))))))

;; (defn with-test-out->str [func]
;;   (let [s (new java.io.StringWriter)]
;;     (str (func))))

;; (defn test-runner-viewer [v]
;;   (with-test-out->str #(t/run v)))


;; (def test-viewer {:transform-fn #(-> % :nextjournal/value :nextjournal.clerk/var-from-def test-runner-viewer)
;;                   :render-fn    '(fn [x]
;;                                    [:span.syntax-string.inspected-value x])})

;; #_(clerk/add-viewers!
;;  [{:pred         test-var?
;;    :transform-fn #(-> % :nextjournal/value :nextjournal.clerk/var-from-def test-runner-viewer)
;;    :render-fn    '(fn [x]
;;                     [:span.syntax-string.inspected-value x])}])

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}
 :clerk/no-cache               true}
(deftest test-2023-10
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/run #'test-2023-10)

(part-1 input-example)



