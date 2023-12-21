^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.20
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.core.match :refer [match]]
            [util :as u]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "20" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn line-parser [l]
  (u/instaparse l "m = type? module <' -> '> dests
                   module = name
                   type = '%' | '&'
                   name = #'[a-z]+'
                   dests = (name <', '?>)+
                   " {:m      merge
                      :type   #(case %
                                 "%" {:type :f}
                                 "&" {:type :c})
                      :module (fn [n]
                                {:module n})
                      :name   str
                      :dests  (fn [& dests]
                                {:dests (into [] dests)})}))


(defn parser [data]
  (->> data
       u/to-lines
       (map line-parser)))

(def input (->> (slurp (io/resource "inputs/2023/20.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a"))

(def input-example-2 (parser "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output"))

(defn get-module [modules name]
  (->> modules
       (filter #(= (:module %) name))
       first))

(defn get-next-queue-items [{:keys [type module dests]} current-pulse on history lasts]
  #_(clojure.pprint/pprint lasts)
  (println module current-pulse lasts on)
  (letfn [(assoc-dests-pulse [pulse]
            (mapv #(vector % pulse) dests))
          (assoc-last-pulse []
            (assoc lasts module current-pulse))]
    (match [type current-pulse]
      [nil _] [(assoc-dests-pulse current-pulse) on (assoc-last-pulse)] ;; broadcaster

      [:f :low] (let [on? (some #(= % module) on)]
                  (if on?
                    [(assoc-dests-pulse :low) (vec (remove #{module} on)) (assoc-last-pulse)]
                    [(assoc-dests-pulse :high) (conj on module) (assoc-last-pulse)]))
      [:f :high] [[] on (assoc-last-pulse)]

      [:c _] (if #_(-> history last :pulse (= :high)) (every? #(= % :high) (vals lasts))
                 [(assoc-dests-pulse :low) on (assoc-last-pulse)]
                 [(assoc-dests-pulse :high) on (assoc-last-pulse)])

      :else [[] on (assoc-last-pulse)]))

  )

;; ## Part 1
(defn part-1
  [modules]

  (->>
   (loop [all-step    0
          all-history []
          all-on      []
          all-lasts   {}]

     (if (= all-step 1)
       (do
         (clojure.pprint/pprint all-history)
         all-history)

       (let [[h o l] (loop [queue   [["broadcaster" :low]]
                            history all-history
                            on      all-on
                            lasts   all-lasts
                            step    0]
                       
                       (if (or #_(> step 100) (empty? queue))
                         [history on lasts]
                         (let [[current pulse]          (first queue)
                               m                        (get-module modules current)
                               [items new-on new-lasts] (get-next-queue-items m pulse on history lasts)]
                           (recur
                            (concat (next queue) items)
                            (conj history {:module current
                                           :pulse  pulse})
                            new-on
                            new-lasts
                            (inc step)))))]
         (recur
          (inc all-step)
          h
          o
          l))))
   (map :pulse)
   frequencies
   (map second)
   (apply *))
  #_(->>

     (map :pulse)
     frequencies
     (map second)
     (map #(* % 1000))
     (apply *))
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


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-20
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

;; 11687500
(part-1 input-example-2)