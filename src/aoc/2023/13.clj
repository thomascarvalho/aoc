^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.13
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.math.numeric-tower :as nt]
            [util :as u]
            [clojure.core.matrix :as ma]
            [test-util :as t]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "13" "2023"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-blocks
       (map #(-> %
                 u/to-matrix
                 ma/matrix))))

(def input (->> (slurp (io/resource "inputs/2023/13.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"))

(def initial-gaps [[0 0] [1 0] [0 1] [1 1] [2 0] [0 2] [1 2] [2 1] [2 2] [3 0] [0 3] [3 1] [1 3] [3 2] [2 3] [3 3] [4 0] [0 4] [4 1] [1 4] [4 2] [2 4] [4 3] [3 4] [4 4]])

(defn palindrome? [vec]
  (let [s (seq vec)]
    (= s (reverse s))))

(defn find-longest-palindrome [vec]
  (let [n           (count vec)
        palindromes (for [i     (range n)
                          j     (range (inc i) (inc n))
                          :let  [subvec (subvec vec i j)]
                          :when (palindrome? subvec)]
                      subvec)]
    #_(println palindromes)
    (apply max-key count palindromes)))


(defn all-subvectors-palindromes [vec]
  (let [n (count vec)]
    (for [start (range n)
          end   (range (inc start) (inc n))
          :let  [s (subvec vec start end)
                 s-count (count s)]
          :when (and (palindrome? s) #_(even? s-count))]
      {:start start
       ;; :mid (+ (/ s-count 2) start)
       :count s-count})))


(defn vertical-reflection [m]
  (let [pals   (for [row m
                     :let [subs (all-subvectors-palindromes row)]
                     :when (seq subs)]
                 (->>
                  subs
                  (apply max-key :count)))
        valid? (apply = pals)]
    (when valid?
      (let [{:keys [start count]} (first pals)]
        [(+ (/ count 2) start) count])))


  #_(let [pals   (for [row m]
                   (all-subvectors-palindromes row))
          valid? (apply = (map count pals))]
      (when valid?
        (let [p (str/join (first pals))
              c (count p)]
          [c (->
              (for [[i part] (map-indexed vector (partition c 1 (first m)))
                    :when    (= p part)]
                (+ (quot c 2) i))
              first)])))
  #_(->>
     (let [col-count    (-> m first count)
           vertical-div 2]
       (loop [gaps initial-gaps
              step 0]
         (when (seq gaps)
           (let [[start end] (first gaps)
                 n           #_(nt/floor)
                 (quot (- col-count start end) vertical-div)vertical?
                 (->>
                  (for [row  m
                        :let [v1 (into [] (drop start (take n row)))
                              v2 (into [] (reverse (take-last (- n start) (drop end row))))]]
                    (compare v1 v2))
                  (every? zero?))]
             (if vertical?
               [(int (+ start n)) step]
               (recur (next gaps) (inc step)))))))))

(defn horizontal-reflection [m]
  (let [pals   (for [row   (ma/columns m)
                     :let  [subs (all-subvectors-palindromes row)]
                     :when (seq subs)]
                 (->>
                  subs
                  (apply max-key :count)))
        valid? (apply = pals)]
    (when valid?
      (let [{:keys [start count]} (first pals)]
        [(+ (/ count 2) start) count])))

  #_(let [pals   (for [row (ma/columns m)]
                   (find-longest-palindrome row))
          valid? (apply = (map count pals))]
      (when valid?
        (let [p (first pals)
              c (count p)]
          [c (->
              (for [[i part] (map-indexed vector (partition c 1 (ma/get-column m 0)))
                    :when    (= p part)]
                (+ (quot c 2) i))
              first)])))
  #_(->>
     (let [row-count      (-> m count)
           horizontal-div 2]

       (loop [gaps initial-gaps
              step 0]
         (when (seq gaps)
           (let [[start end] (first gaps)
                 n           (quot (- row-count start end) horizontal-div)
                 v1          (into [] (drop start (take n m)))
                 v2          (into [] (reverse (take-last (- n start) (drop end m))))]
             (if (zero? (compare v1 v2))
               [(int (+ start n)) step]
               (recur (next gaps) (inc step))))))


    ;;  (when horizontal?
    ;;    (int (+ horizontal-from (nt/floor (/ row-count horizontal-div)))))
       )))

(defn process-matrix [m]
  (let [[v v-count] (vertical-reflection m)
        [h h-count] (horizontal-reflection m)]
    #_(println [v h])
    [(or v 0)  (or h 0)]
    #_(cond
      (and v (not h)) [v 0]
      (and h (not v)) [0 h]
      (and v-count h-count (> v-count h-count)) [v 0]
      (and v-count h-count (< v-count h-count)) [0 h]
      :else (do 
              #_(println [v h])
              [0 0]))
    #_(if (< v-step h-step)

        [v 0 {:v      v
              :h      h
              :v-step v-step
              :h-step h-step}]
        [0 h {:v      v
              :h      h
              :v-step v-step
              :h-step h-step}])))

;; ## Part 1
(defn part-1
  [all-matrices]

  #_(for [m all-matrices]
      (process-matrix m))

  (let [[v h] (->>
               all-matrices
               (reduce (fn [[v-total h-total] m]
                         (let [[v h] (process-matrix m)]
                           [(+ v-total v) (+ h-total h)])) [0 0]))]

    (+ v (* h 100)))
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
(deftest test-2023-13
  (testing "part one - example"
    (is (= 405 (part-1 input-example))))

  #_(testing "part one"
      (is (= 1 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
;; ## Results
#_(t/render-results (t/run #'test-2023-13))

(part-1 input)