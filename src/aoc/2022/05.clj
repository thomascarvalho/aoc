(ns aoc.2022.05
  (:require [clojure.test :refer :all]
            [util :refer [parse-int read-from-ns]]
            [clojure.string :as cs]))

(def input (read-from-ns ::x))

(def input-example "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parser [data]
  (let [[raw-stacks raw-instructions] (cs/split data #"\n 1")
        stacks                        (atom {})
        clean-stacks                  (->>  raw-stacks
                                            cs/split-lines
                                            (mapcat (fn [l]
                                                      (let [p (partition 4 4 "" l)]
                                                        (->>
                                                         (map-indexed (fn [index [_ letter]]
                                                                        (when-not (= letter \space) [index letter])) p)
                                                         (remove nil?))))))]

    (doseq [[index letter] clean-stacks]
      (swap! stacks update (keyword (str (inc index))) conj letter))
    {:stacks       stacks
     :instructions (->>
                    (cs/split-lines raw-instructions)
                    (map (fn [instruction]
                           (let [[_ nb from to] (re-matches #"move (\d+) from (\d+) to (\d+)" instruction)]
                             (when nb {:nb   (parse-int nb)
                                       :from (keyword from)
                                       :to   (keyword to)}))))
                    (remove nil?))}))

(defn action
  [stacks {:keys [nb from to]} {:keys [keep-order?]}]
  (let [from-stack     (get @stacks from)
        elements       (take-last nb from-stack)
        new-from-stack (drop-last nb from-stack)]
    (swap! stacks assoc from new-from-stack)
    (swap! stacks update to concat (if keep-order? elements (reverse elements)))))

(defn part-one
  ([]
   (part-one {:keep-order? false}))
  ([{:keys [keep-order?]}]
   (let [{:keys [stacks instructions]} (parser input)]
     (doseq [instruction instructions]
       (action stacks instruction {:keep-order? keep-order?}))
     (-> (for [[_index letters] (sort @stacks)]
           (last letters))
         cs/join))))

(defn part-two []
  (part-one {:keep-order? true}))

(deftest test-2022-05
  (testing "part one"
    (is (= "WCZTHTMPS" (part-one))))

  (testing "part two"
    (is (= "BLSGJSDTS" (part-two)))))








