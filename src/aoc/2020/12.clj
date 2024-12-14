^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2020.12
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int manhattan-distance]]
            [clojure.string :as str]
            [clojure.test :refer :all]))


(def directions-mapping {:E [1 0]
                         :S [0 1]
                         :W [-1 0]
                         :N [0 -1]})

(def cardinal-directions [:E :S :W :N])

(defn turn [directions degrees turn]
  (let [s (case degrees
            90 1
            180 2
            270 3)]
    (->
     (case turn
       :R (next (drop (dec s) (take (+ 4 s) (cycle directions))))
       :L (concat (drop (+ 2 s) directions) (take (+ 2 s) directions)))
     vec)))

(defn move [initial-coords steps directions]
  (let [[x2 y2] (if (vector? directions) (first directions) (directions-mapping directions))]
    (loop [[final-x final-y] initial-coords
           current-steps     steps]
      (if (zero? current-steps)
        [final-x final-y]
        (recur [(+ final-x x2) (+ final-y y2)] (dec current-steps))))))


(defn neighbours [[x y] directions]
  (map (fn [dir]
         (let [[x2 y2] (directions-mapping dir)]
           [(+ x x2) (+ y y2)])) directions))
;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map #(let [[_ action n] (re-find #"([A-Z])(\d+)" %)]
               [(keyword action) (parse-int n)]))))

(def input (->> (slurp (io/resource "inputs/2020/12.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "F10
N3
F7
R90
F11"))

;; ## Part 1
(defn part-1
  [data]

  (->
   (loop [actions    data
          pos        [0 0]
          directions [:E :S :W :N]]

     (if (seq actions)
       (let [[t n] (first actions)]
         (cond
           (some #(= t %) [:N :E :S :W]) (recur (next actions) (move pos n t) directions)
           (some #(= t %) [:R :L]) (recur (next actions) pos (turn directions n t))
           (= :F t) (recur (next actions) (move pos n (first directions)) directions)))
       pos))
  ;
     
   (manhattan-distance [0 0])))


;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [input])

  ;
  


;; Tests
(deftest test-2020-12

  (testing "turn"
    (is (= [:N :E :S :W] (turn [:E :S :W :N] 90 :L)))
    (is (= [:E :S :W :N] (turn [:E :S :W :N] 180 :L)))
    (is (= [:W :N :E :S] (turn [:N :E :S :W] 90 :L)))

    (is (= [:S :W :N :E] (turn [:E :S :W :N] 90 :R)))
    (is (= [:E :S :W :N] (turn [:N :E :S :W] 90 :R)))

    (is (= [:S :W :N :E] (turn [:N :E :S :W] 180 :R)))
    (is (= [:W :N :E :S] (turn [:N :E :S :W] 270 :R))))



  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

