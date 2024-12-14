^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.12
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.pprint :refer [pprint]]
            [instaparse.core :as insta]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map #(let [[t nums] (str/split % #" ")]
               [t (u/parse-out-longs nums)]))))

(def input (->> (slurp (io/resource "inputs/2023/12.txt"))
                parser))
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser
                    "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"))

(defn damaged-iterator [t from length]

  (let [r (->> (partition length 1 t)
               (map-indexed (fn [i g] [i g]))
               (filter (fn [[i group]] (every? #(= % \#) group))))]
    (when (= 1 (count r)) (first r))))

(let [[l n] ["???.###" '(1 1 3)]]

  (u/re-pos #"\#+" l))
(defn get-groups [data]
  (->> data
       (map (fn [[initial lengths]]
              {:initial initial
               :lengths lengths
               :groups (str/split initial #"\.+")}))))

(def regex #"((.)\2*)")

(defn split-string [s]
  (map first (re-seq regex s)))
(defn decode [data]
  (->> data
       (map (fn [[line lengths]]
              {:initial line
               :lengths lengths
               :groups (loop [[group & groups] (split-string line)
                              x 0
                              res []]
                         (if group
                           (recur groups (+ x (count group)) (conj res {:type (first group)
                                                                        :n (count group)
                                                                        :x x}))
                           res))}))
       #_get-groups
       #_(map (fn [{:keys [lengths groups] :as data}]
                (let [valid-groups-count? (= (count lengths) (count groups))]
                  (assoc data :valid-groups-count? valid-groups-count?))))
       #_(map (fn [{:keys [valid-groups-count? lengths groups] :as data}]
                (if valid-groups-count?)))))
;; ## Part 1
(defn part-1
  [data]
  (let [data (->> data decode)]
    (for [{:keys [lengths groups] :as line} data]
      (assoc line :groups (reduce (fn [grps]) [] groups)))))



;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input])

  ;



;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
#_(deftest test-2023-12
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

