^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2022.21
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [ubergraph.core :as uber]
            [clojure.math.numeric-tower :as math]
            [clojure.pprint :refer [pprint]]
            [ubergraph.alg :as alg]
            [nextjournal.clerk :as clerk]
            [util :as u :refer [parse-int]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
(clerk/html (u/load-problem "21" "2022"))
{:nextjournal.clerk/visibility {:code   :show
                                :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       str/split-lines
       (map (fn [d]
              (->> d
                   ((insta/parser
                     "S = monkey <': '> action
                      <action> = (yells | monkeys)
                      int = #'-?\\d+'
                      yells = int
                      monkey = #'[a-z]+'
                      operator = #'[/*\\-+]'
                      monkeys = (monkey <' '> operator <' '> monkey)"))
                   #_{:clj-kondo/ignore [:unresolved-var]}
                   (insta/transform {:int      parse-int
                                     :operator (fn [o] (eval (symbol o)))
                                     :monkey   str
                                     :yells    (fn [n]
                                                 {:value n})
                                     :monkeys  (fn [& [m1 op m2]]
                                                 {:monkeys  [m1 m2]
                                                  :operator op})
                                     :S        vector}))))
       #_(into [])))

(def input (->> (slurp (io/resource "inputs/2022/21.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"))

;; ## Part 1
(defn part-1
  [data]
  (let [edges               (for [[monkey {:keys [monkeys]}] (filter #(get (second %) :monkeys) data)
                                  associated-monkey          monkeys]
                              [monkey associated-monkey])
        g                   (-> (uber/digraph)
                                (uber/add-edges* edges))
        initial-monkeys-map (->> data
                                 (map (fn [[m opts]]
                                        (hash-map m opts)))
                                 (apply merge))]
    (->
     (loop [traverse    (alg/post-traverse g "root")
            monkeys-map initial-monkeys-map]
       (if (seq traverse)
         (let [monkey-name (first traverse)
               monkey      (get monkeys-map monkey-name)]
           (if (:value monkey)
             (recur (next traverse) monkeys-map)

             (recur
              (next traverse)
              (assoc-in monkeys-map [monkey-name :value]
                        ((:operator monkey)
                         (get-in monkeys-map [(first (:monkeys monkey)) :value])
                         (get-in monkeys-map [(second (:monkeys monkey)) :value]))))))


         monkeys-map))
     (get-in ["root" :value]))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-1 input)

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}

(def min-low  0 #_(math/expt 10 1000) #_(math/expt 1 1))
(def max-high (math/expt 10 1000) #_(math/expt 100 100000))

(defn create-graph [data]
  (let [edges (for [[monkey {:keys [monkeys]}] (filter #(get (second %) :monkeys) data)
                    associated-monkey          monkeys]
                [monkey associated-monkey])]
    (-> (uber/digraph)
        (uber/add-edges* edges))))

(defn create-monkeys-map [data]
  (->> data
       (map (fn [[m opts]] (hash-map m opts)))
       (apply merge)))

(defn assoc-value [m-map name value]
  (assoc-in m-map [name :value] value))

(defn part-2
  [data]
  (let [g                   (create-graph data)
        initial-monkeys-map (-> data
                                create-monkeys-map
                                (assoc-in ["root" :operator] +)
                                (assoc-value "humn" (quot (+ min-low max-high) 2)))
        initial-traverse    (alg/post-traverse g "root")]
    (loop [traverse       initial-traverse
           monkeys-map    initial-monkeys-map
           my-value-range [min-low max-high]]
      (letfn [(get-value [name] (get-in monkeys-map [name :value]))]
        (let [monkey-name (first traverse)
              monkey      (get monkeys-map monkey-name)
              [m1 m2]     (:monkeys monkey)]
          (cond
            ;; End of the traverse, check if my value make it match
            (= monkey-name "root")
            (let [diff       (compare (get-value m1) (get-value m2))
                  my-value   (get-value "humn")
                  [low high] my-value-range
                  mid        (quot (+ low high) 2)]
              (cond
                ;; Success
                (= diff 0) my-value
                ;; Issue on the range, too low
                (and 1 #_(= diff -1) (= my-value max-high)) (throw (Exception. (str "Max range limit, diff=" diff)))
                ;; Too low
                (= diff -1) (recur
                             initial-traverse
                             (assoc-value initial-monkeys-map "humn" (quot (+ (inc mid) high) 2))
                             [(inc mid) high])
                ;; Too high 
                :else (recur
                       initial-traverse
                       (assoc-value initial-monkeys-map "humn" (quot (+ low (dec mid)) 2))
                       [low (dec mid)])))

            ;; Value already present -> next node
            (:value monkey)
            (recur
             (next traverse)
             monkeys-map
             my-value-range)

            ;; Calculate and assoc value based on previous predecessors monkeys
            :else
            (recur
             (next traverse)
             (assoc-value monkeys-map monkey-name ((:operator monkey)
                                                   (get-value (first (:monkeys monkey)))
                                                   (get-value (second (:monkeys monkey)))))
             my-value-range)))))))

;; Which gives our answer
{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}
#_(part-2 input)


;; Tests
(deftest test-2022-21
  (testing "part one"
    (is (= 75147370123646 (part-1 input))))

  #_(testing "part two"
    (is (= 1 (part-2 input)))))

#_(time (part-2 input))