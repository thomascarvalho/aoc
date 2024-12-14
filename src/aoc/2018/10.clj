^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2018.10
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [pp-grid.api :as g]
            [test-util :as t]
            [util :as u]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "10" "2018"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (mapv #(into [] (u/parse-out-longs %)))))

(def input (->> (slurp (io/resource "inputs/2018/10.txt")) ;; Load the resource
                parser))

{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>"))

(defn draw [points]
  (->> points
       (reduce (fn [acc [x y]]
                 (assoc acc [x y] \#))
               (g/empty-grid))))

(defn- move [[x y right up]]
  [(+ x right) (+ y up) right up])

(defn- neighbours
  ([points]
   (mapv #(conj % (neighbours points %)) points))
  ([points [x y]]
   (->> points
        (filter (fn [[tx ty]]
                  (some #(= % [tx ty]) [[x (inc y)] [x (dec y)]
                                        [(dec x) y] [(inc x) y]
                                        [(dec x) (inc y)] [(dec x) (dec y)]
                                        [(inc x) (inc y)] [(inc x) (dec y)]]))))))

;; ## Part 1
(defn part-1
  [data]
  (-> (loop [points data
             step 0]
        (if (or (every? (fn [[_ _ _ _ current-neighbours]]
                          (seq current-neighbours)) (neighbours points))
                (= step 10000000))
          points
          (let [new-points (mapv move points)]
            (recur
             new-points
             (inc step)))))
      draw
      str))

;; ## Part-2
(defn part-2
  [data]
  (loop [points data
         step 0]
    (if (or (every? (fn [[_ _ _ _ current-neighbours]]
                      (seq current-neighbours)) (neighbours points))
            (= step 10000000))
      step
      (let [new-points (mapv move points)]
        (recur
         new-points
         (inc step))))))

(deftest test-2018-10
  (testing "part one"
    (is (= "#####   #####   #    #  #    #  #    #  ######  ######  ##### \n#    #  #    #  ##   #  ##   #  #    #  #            #  #    #\n#    #  #    #  ##   #  ##   #   #  #   #            #  #    #\n#    #  #    #  # #  #  # #  #   #  #   #           #   #    #\n#####   #####   # #  #  # #  #    ##    #####      #    ##### \n#  #    #       #  # #  #  # #    ##    #         #     #  #  \n#   #   #       #  # #  #  # #   #  #   #        #      #   # \n#   #   #       #   ##  #   ##   #  #   #       #       #   # \n#    #  #       #   ##  #   ##  #    #  #       #       #    #\n#    #  #       #    #  #    #  #    #  #       ######  #    #"
           (part-1 input)))

    (testing "part two"
      (is (= 10946 (part-2 input))))))

