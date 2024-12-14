(ns aoc.2024.09
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [medley.core :as m]
            [clojure.test :refer [deftest is testing]]))

;; # Parser
(defn parser [data]
  (->> (re-seq #"\d" data)
       (mapv parse-long)
       (map-indexed (fn [i v] [i v]))
       (reduce (fn [{:keys [id] :as m} [i v]]
                 (if (even? i)
                   (-> m
                       (update :blocks conj (repeat v  id))
                       (update :id inc))
                   (update m :blocks conj (repeat v "."))))
               {:id 0
                :blocks []})
       :blocks
       flatten
       (into [])))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/09.txt"))
                parser))

(def input-example (parser "2333133121414131402"))

;; Logic

(defn indexed [coll]
  (map-indexed (fn [i v] [i v]) coll))

(defn get-first-free-space-index [blocks start]
  (+ start (reduce (fn [_ [i v]]
                     (when (= v ".")
                       (reduced i)))
                   (indexed (drop start blocks)))))

(defn get-checksum [blocks]
  (->> blocks
       (keep-indexed
        (fn [i v]
          (when (not= v ".")
            (* i v))))
       (reduce +)))

;; ## Part 1
(defn part-1
  [data]
  (let [blocks data]
    (loop [blocks blocks
           start-non-empty-index 0
           current-index (dec (count blocks))]
      (let [current-value (get blocks current-index)]
        (if (= current-value ".")
          (recur
           blocks
           start-non-empty-index
           (dec current-index))
          (let [free-index (get-first-free-space-index blocks start-non-empty-index)]
            (cond
              (< free-index current-index)
              (recur
               (-> blocks
                   (assoc free-index current-value)
                   (assoc current-index "."))
               free-index
               (dec current-index))

              :else (get-checksum blocks))))))))

;; ## Part 2
(defn indexed-groups [blocks]
  (loop [[[i b :as current-block] & indexed-blocks] (indexed blocks)
         current-group nil
         all-groups (into (sorted-map) {})]
    (if current-block
      (if (= (:value current-group) b)
        (recur
         indexed-blocks
         (update current-group :count inc)
         all-groups)

        (recur
         indexed-blocks
         {:index i :value b :count 1}
         (if current-group
           (assoc all-groups (:index current-group) (dissoc current-group :index))
           all-groups)))
      (assoc all-groups (:index current-group) (dissoc current-group :index)))))

(defn find-first-free-block-available [grouped-blocks [right-index v]]
  (first (filter (fn [[index v2]]
                   (and (= (:value v2) ".")
                        (<= index right-index)
                        (>= (:count v2) (:count v))))
                 grouped-blocks)))

(defn replace-blocks [blocks source target]
  (let [[i-source v-source] source
        [i-target v-target] target
        diff (- (:count v-target) (:count v-source))]
    (cond-> blocks
      :always (assoc i-target v-source)
      :always (assoc i-source (assoc v-target :count (:count v-source)))
      (pos? diff)
      (assoc (+ i-target (:count v-source)) (assoc v-target :count diff)))))

(defn part-2
  [data]
  (let [blocks data
        grouped-blocks (indexed-groups blocks)]
    (->> (loop [blocks grouped-blocks
                grouped-blocks grouped-blocks]
           (if-let [source (last blocks)]
             (if (not= (:value (second source)) ".")
               (if-let [target-blocks (find-first-free-block-available grouped-blocks source)]
                 (recur
                  (butlast blocks)
                  (replace-blocks grouped-blocks source target-blocks))
                 (recur (butlast blocks)
                        grouped-blocks))
               (recur (butlast blocks)
                      grouped-blocks))
             grouped-blocks))
         (mapcat (fn [[_ {:keys [value count]}]]
                   (repeat count value)))
         get-checksum)))

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2024-09
  (testing "part one"
    (is (= 6446899523367 (part-1 input))))

  (testing "part two - example"
    (is (= 2858 (part-2 input-example))))
  (testing "part two"
    (is (= 6478232739671 (part-2 input)))))

