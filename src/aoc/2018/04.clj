^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.04
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "04" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       sort
       (reduce (fn [{:keys [current-guard-id] :as m} l]
                 (let [[_ year month' day' hour' minute' action'] (re-find #"\[(\d+)-(\d+)-(\d+) (\d+)\:(\d+)\] (.*)" l)
                       action (str/trim action')
                       [month day hour minute] (mapv u/parse-int [month' day' hour' minute'])
                       m' (update m :years (comp set conj) year)]
                   (case action
                     "falls asleep" (assoc-in m' [:guards current-guard-id month day hour minute] :sleep)
                     "wakes up" (assoc-in m' [:guards current-guard-id month day hour minute] :wakes-up)
                     (let [id (first (u/parse-out-longs action))]
                       (-> m'
                           (assoc :current-guard-id id)
                           (assoc-in [:guards id month day hour minute] :begins))))))

               {:years []
                :current-guard-id nil
                :guards {}})
       #_:years :guards))


(def input (->> (slurp (io/resource "inputs/2018/04.txt")) ;; Load the resource
                parser))

                                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"))

;; ## Part 1
(defn part-1
  [input]
  (let [m input
        data (for [[guard-id month] m]
               (->> (for [[_ day] month
                          [_ hour] day
                          [_ minutes] hour]
                      (->> minutes
                           (reduce (fn [{:keys [last-sleep-minute] :as m} [minute action]]
                                     (case action
                                       :sleep (assoc m :last-sleep-minute minute)
                                       :wakes-up (update m :sleeping-ranges conj (into [](range last-sleep-minute  minute)))
                                       m))
                                   {:last-sleep-minute nil
                                    :sleeping-ranges []})
                           :sleeping-ranges))
                    flatten
                    (vector guard-id)))
        best-sleeper-id (->> (for [[guard-id sleeping-minutes] data]
                               [guard-id (count sleeping-minutes)])
                             (sort-by second >)
                             ffirst)
        most-sleeped-minute (->> data
                                 (filter (fn [[guard-id]]
                                           (= guard-id best-sleeper-id)))
                                 first
                                 second
                                 frequencies
                                 (sort-by val >)
                                 ffirst)]
    (* best-sleeper-id most-sleeped-minute)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part-2
  [data]
  data)
(let [m input
      data (for [[guard-id month] m]
             (->> (for [[_ day] month
                        [_ hour] day
                        [_ minutes] hour]
                    (->> minutes
                         (reduce (fn [{:keys [last-sleep-minute] :as m} [minute action]]
                                   (case action
                                     :sleep (assoc m :last-sleep-minute minute)
                                     :wakes-up (update m :sleeping-ranges conj (into [] (range last-sleep-minute minute)))
                                     m))
                                 {:last-sleep-minute nil
                                  :sleeping-ranges []})
                         :sleeping-ranges))
                  flatten
                  (into [])
                  (vector guard-id)))]
  (->> data
       (map (fn [[id minutes]]
              [id (->> (frequencies minutes)
                       (sort-by val >)
                       ffirst)]))
       (reduce (fn [[_ current-max :as current] [id minute]]
                 (if (> (or minute 0) current-max)
                   [id minute]
                   current))
               [0 0])
       (apply *)))
;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2018-04
  (testing "part one"
    (is (= 103720 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

