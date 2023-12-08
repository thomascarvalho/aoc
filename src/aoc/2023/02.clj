^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.02
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "02" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map #(let [[_ id rest] (re-find #"Game (\d+): (.*)" %)]
               [(parse-int id)
                (map (fn [rest]
                       (let [boxes (re-seq #"(\d+) (\w+)" rest)]

                         (map (fn [[_ num color]]
                                [(parse-int num) (keyword color)]) boxes))) (str/split rest #";"))]))))

(def input (->> (slurp (io/resource "inputs/2023/02.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

;; ## Part 1
(defn part-1
  [data]
  (->>
   ;; 12 red cubes, 13 green cubes, and 14 blue cubes
   data
   (map (fn [[id boxes]]
          [id (mapcat concat boxes)]))
   (map (fn [[id boxes]]
          {:id    id
           :blue  (->>
                   (filter (fn [[_ color]]
                             (= color :blue)) boxes)
                   (map first))
           :red   (->>
                   (filter (fn [[_ color]]
                             (= color :red)) boxes)
                   (map first))
           :green (->>
                   (filter (fn [[_ color]]
                             (= color :green)) boxes)
                   (map first))}))
   (filter (fn [{:keys [blue green red]}]
             (and
              (every? #(<= % 14) blue)
              (every? #(<= % 12) red)
              (every? #(<= % 13) green))))
   (map #(get % :id))
   (reduce +)))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  (->>
   data
   (map (fn [[id boxes]]
          [id (mapcat concat boxes)]))
   (map (fn [[id boxes]]
          {:id    id
           :blue  (->>
                   (filter (fn [[n color]]
                             (= color :blue)) boxes)
                   (map first))
           :red   (->>
                   (filter (fn [[n color]]
                             (= color :red)) boxes)
                   (map first))
           :green (->>
                   (filter (fn [[n color]]
                             (= color :green)) boxes)
                   (map first))}))
   (map (fn [{:keys [blue green red]}]
          (let [blue  (reduce max blue)
                red   (reduce max red)
                green (reduce max green)]
            (*  blue red   green))))

   (reduce +))

  ;
  )

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(part-2 input)


;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

;; ## Suite
(deftest test-2023-02
  (testing "part one"
      (is (= 2486 (part-1 input))))

  (testing "part two"
    (is (= 87984 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
(t/render-results (t/run #'test-2023-02))