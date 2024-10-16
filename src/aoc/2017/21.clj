^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.21
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.core.matrix :as m]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "21" "2017"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (map (fn [l]
              (let [in-and-out (str/split l #" => ")]
                (mapv (fn [p]
                        (mapv #(char-array %) (str/split p #"/"))) in-and-out))))))

(def input (->> (slurp (io/resource "inputs/2017/21.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#"))

(def big-program [[\# \# \. \# \# \.]
                  [\# \. \. \# \. \.]
                  [\. \. \. \. \. \.]
                  [\# \# \. \# \# \.]
                  [\# \. \. \# \. \.]
                  [\. \. \. \. \. \.]])

(defn split-program [program]
  (let [size            (count program)
        divisible-by-3? (zero? (mod size 3))
        subsize         (if divisible-by-3? 3 2)
        ranges          (partition subsize (range size))] 
    
    (for [[first-y] ranges
          [first-x] ranges]
      (-> (m/submatrix program [[first-y subsize] [first-x subsize]])
          (m/matrix)
          ))))




#_(defn transform-squares [squares rules]
  
  )


(let [pattern (m/matrix [[\. \# \.]
                         [\. \. \#]
                         [\# \# \#]])]

  (->
   (m/rotate pattern 10 2)
   m/pm)
  )

(let [squares (split-program big-program)
      rules   input-example]

  (map (fn [s]
         (prn s)
         (.indexOf rules s)) squares))


(let [rules input-example]
  (loop [program [[\. \# \.]
                  [\. \. \#]
                  [\# \# \#]]
         step    0]
    
    (let [size            (count program)
          divisible-by-3? (zero? (mod size 3))]

      (if divisible-by-3?
        (let [rows (partition 3 program)]
          (map #(partition 3 %) rows))
        nil))
    
    #_(pprint program)
    
    ;;
    ))

;; ## Part 1
(defn part-1
  [data]
  data)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-21
  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/test-render #'test-2017-21)