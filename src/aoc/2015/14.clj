^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2015.14
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "14" "2015"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map (fn [l]
              (let [[_ name speed travel-time rest-time] (re-find #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds." l)]
                {:name        name
                 :speed       (parse-long speed)
                 :travel-time (parse-long travel-time)
                 :rest-time   (parse-long rest-time)})))
       (map #(assoc %
                    :current-distance 0
                    :current-travel-time (:travel-time %)
                    :current-rest-time (:rest-time %)
                    :flying? true))))

(def input (->> (slurp (io/resource "inputs/2015/14.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."))

;; ## Part 1
(defn part-1
  [comets max-seconds]
  comets
  #_(->>
     (loop [comets  comets
            seconds 0]

       (if (= seconds max-seconds)
         comets

         (recur
          (->> comets
               (reduce (fn [v {:keys [flying? speed travel-time rest-time current-travel-time current-distance current-rest-time]
                               :as   comet}]
                         (let [new-comet
                               (cond-> comet
                                 (and flying? (zero? current-travel-time)) (assoc
                                                                            :flying? false
                                                                            :current-travel-time travel-time
                                                                            :current-rest-time (dec current-rest-time))

                                 (and flying? (> current-travel-time 0)) (->
                                                                          (update :current-distance + speed)
                                                                          (update :current-travel-time dec))

                                 (and (not flying?) (zero? current-rest-time)) (assoc
                                                                                :flying? true
                                                                                :current-rest-time rest-time)

                                 (and (not flying?) (> current-rest-time 0)) (->
                                                                              (update :current-rest-time dec)))]


                           (conj v new-comet))) []))
          (inc seconds))))
     #_(reduce (fn [t {:keys [current-distance]}]
                 (max t current-distance)) 0)))
  ;
  

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  data)

  ;
  

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2015-14
  (testing "part one"
    (is (= 1120 (part-1 input-example 1000))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results

;; 2530, 2585 too low

#_(part-1 input 2503)
#_(part-1 input-example 1000)


