^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2023.13
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [clojure.math.numeric-tower :as nt]
            [util :as u]
            [clojure.core.matrix :as ma]
            [clojure.string :as str]
            [clojure.test :refer :all]))


;; # Solution

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
(def input-example (parser
                    "#.##..##.
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
    (apply max-key count palindromes)))

(defn all-subvectors-palindromes [vec]
  (let [n (count vec)]

    (into []
          (concat
           (for [start (range n)
                 end   (range (inc start) (inc n))
                 :let  [s (subvec vec start end)
                        s-count (count s)]
                 :when (and (palindrome? s) (even? s-count))]
             {:start start
              :end   end
              :count s-count})

           (for [start [0]
                 end   (range n 0 -1)
          ;; end   (range (inc start) (inc n))
                 :let  [s (subvec vec start end)
                        s-count (count s)]
                 :when (and (palindrome? s) (even? s-count))]
             {:start start
              :end   end
              :count s-count})))))

    ;
    


(defn vertical-reflection [m]
  (let [pals   (for [row   m
                     :let  [subs (all-subvectors-palindromes row)]
                     :when (seq subs)]
                 (->>
                  subs
                  (apply max-key :count)))
        valid? (apply = pals)]
    (when valid?
      (let [{:keys [start count]} (first pals)]
        [(+ (/ count 2) start) count]))))

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
               (recur (next gaps) (inc step)))))))))


    ;;  (when horizontal?
    ;;    (int (+ horizontal-from (nt/floor (/ row-count horizontal-div)))))
       

(defn process-matrix [m]
  ;; (ma/pm m)
  ;; (let [row-start  0
  ;;       row-length (- (count m) row-start)
  ;;       col-start  1
  ;;       col-length (- (count (first m)) col-start)
  ;;       sub-m      (ma/submatrix m row-start row-length col-start col-length)
  ;;       v1         (take (quot col-length 2) sub-m)]
  ;;   (ma/pm sub-m)
  ;;   (ma/pm v1))
  [(or (vertical-reflection m) [0 0]) (or (horizontal-reflection m) [0 0])]

  #_(->>
     (all-subvectors-palindromes m)
     (sort-by :count >)))
  ;
  

;; ## Part 1
(defn part-1
  [all-matrices]

  #_(for [m all-matrices]
     (process-matrix m))

  (let [[v h] (->>
               all-matrices
               (reduce (fn [[v-total h-total] m]
                         (let [[v h] (process-matrix m)]
                           [(+ v-total (first v)) (+ h-total (first h))])) [0 0]))]

    (+ v (* h 100))))
  ;
  

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
#_(deftest test-2023-13
    #_(testing "part one - example"
        (is (= 405 (part-1 input-example))))

    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))
