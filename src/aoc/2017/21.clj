^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.21
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
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
  (let [program big-program
        size            (count program)
        divisible-by-3? (zero? (mod size 3))]
  
    #_(if divisible-by-3?
      (let [ranges (partition 3 (range size))]
        ranges
        (for [y-group ranges]
          (for [x-group ranges]
            (mapv #() x-group))
          )
        )
      #_(let [rows (into [] (partition 3 program))]
        (for [r rows]
          (mapv #(partition 3 %) r)
          )

        #_(nth rows 0)
        #_(map #(partition 3 %) rows)
        ;; rows
        )
      nil))
  )


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