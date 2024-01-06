^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.19
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "19" "2015"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (let [all (u/to-lines data)]
    [(->> all
          (drop-last 2)
          (mapv (fn [s]
                  (str/split s #" => "))))
     (last all)]))

(def input (->> (slurp (io/resource "inputs/2015/19.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "H => HO
H => OH
O => HH

HOH"))

;; ## Part 1
(defn part-1
  [[replacements molecule]]
  (->>
   replacements
   (mapcat
    (fn [[t r]]
      (->> molecule
           (u/re-pos (re-pattern t))
           (map (fn [[pos s]]
                  (str (subs molecule 0 pos) (str/replace-first (subs molecule pos) s r)))))))
   set
   count))

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
(deftest test-2015-19
  (testing "part one"
      (is (= 576 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

#_(part-1 input)