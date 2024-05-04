^{:nextjournal.clerk/visibility :hide-ns}
(ns aoc.2017.18
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [test-util :as t]
            [util :as u]
            [clojure.core.async :as async :refer [chan go]]
            [clojure.string :as str]
            [clojure.test :refer :all]))

;; # Problem
;; {:nextjournal.clerk/visibility {:code :hide :result :show}}
;; (clerk/html (u/load-problem "18" "2017"))
;; {:nextjournal.clerk/visibility {:code :show :result :show}}

;; # Solution
;;
;; First things first, let's load our input and parse it

(defn parser [data]
  (->> data
       u/to-lines
       (mapv #(let [[action & vars] (str/split % #" ")]
                [(keyword action) (mapv (fn [v] (let [v (read-string v)]
                                                  (if (number? v)
                                                    v
                                                    (keyword v)))) vars)]))))

(def input (->> (slurp (io/resource "inputs/2017/18.txt")) ;; Load the resource
                parser))                             ;; Split into lines
{:nextjournal.clerk/visibility {:result :hide}}

;;  Example
(def input-example (parser "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"))

(defn get-v [reg v]
  (if (keyword? v)
    (get reg v 0)
    v))


;; ## Part 1
(defn part-1
  [data]
  (loop [idx        0
         reg        {}
         last-sound nil]

    (if-let [[action [x y]] (get data idx)]
      (letfn [(v [t]
                (get-v reg t))]
        (case action
          :set (recur
                (inc idx)
                (assoc reg x (v y))
                last-sound)

          :snd (recur
                (inc idx)
                reg
                (v x))

          :add (recur
                (inc idx)
                (assoc reg x (+ (v x) (v y)))
                last-sound)

          :mul (recur
                (inc idx)
                (assoc reg x (* (v x) (v y)))
                last-sound)

          :mod (recur
                (inc idx)
                (assoc reg x (mod (v x) (v y)))
                last-sound)

          :rcv (if (zero? (v x))
                 (recur
                  (inc idx)
                  reg
                  last-sound)

                   ;; return last sound, stop
                 last-sound)

          :jgz (if (pos? (v x))
                 (recur
                  (+ idx (v y))
                  reg
                  last-sound)

                 (recur
                  (inc idx)
                  reg
                  last-sound))))
      last-sound)))

;; ## Part 2
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
#_(let [data (parser "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")
      p0   (chan)
      p1   (chan)]
  (letfn [(program [id]
                   (prn id)
                   (let [queue (atom [])]
                     (go (while true (let [v (async/<! (if (zero? id) p0 p1))]
                                       (prn v)
                                       (swap! queue conj v))))
                     (loop [idx        0
                            reg        {:p id}
                            last-sound nil]
                       (println id @queue)
                       (if-let [[action [x y]] (get data idx)]
                         (letfn [(v [t]
                                   (get-v reg t))]
                           (case action
                             :set (recur
                                   (inc idx)
                                   (assoc reg x (v y))
                                   last-sound)

                             :snd (do
                                    (go (if (zero? id)
                                          (async/>! p1 (v x))
                                          (async/>! p0 (v x))))
                                    (recur
                                     (inc idx)
                                     reg
                                     (v x)))

                             :add (recur
                                   (inc idx)
                                   (assoc reg x (+ (v x) (v y)))
                                   last-sound)

                             :mul (recur
                                   (inc idx)
                                   (assoc reg x (* (v x) (v y)))
                                   last-sound)

                             :mod (recur
                                   (inc idx)
                                   (assoc reg x (mod (v x) (v y)))
                                   last-sound)

                             :rcv
                             (if (zero? (v x))
                               (recur
                                (inc idx)
                                reg
                                last-sound)





                   ;; return last sound, stop
                               last-sound)

                             :jgz (if (pos? (v x))
                                    (recur
                                     (+ idx (v y))
                                     reg
                                     last-sound)

                                    (recur
                                     (inc idx)
                                     reg
                                     last-sound))))
                         last-sound))))]

    (dotimes [id 2]
      (go
        (loop []
          (let [queue (atom [])]
            (go (while true (let [v (async/<! (if (zero? id) p0 p1))]
                              (prn v)
                              (swap! queue conj v))))
            (loop [idx        0
                   reg        {:p id}
                   last-sound nil]
              (println id @queue)
              (if-let [[action [x y]] (get data idx)]
                (letfn [(v [t]
                          (get-v reg t))]
                  (case action
                    :set (recur
                          (inc idx)
                          (assoc reg x (v y))
                          last-sound)

                    :snd (do
                           (go (if (zero? id)
                                 (async/>! p1 (v x))
                                 (async/>! p0 (v x))))
                           (recur
                            (inc idx)
                            reg
                            (v x)))

                    :add (recur
                          (inc idx)
                          (assoc reg x (+ (v x) (v y)))
                          last-sound)

                    :mul (recur
                          (inc idx)
                          (assoc reg x (* (v x) (v y)))
                          last-sound)

                    :mod (recur
                          (inc idx)
                          (assoc reg x (mod (v x) (v y)))
                          last-sound)

                    :rcv
                    (if (zero? (v x))
                      (recur
                       (inc idx)
                       reg
                       last-sound)





                             ;; return last sound, stop
                      last-sound)

                    :jgz (if (pos? (v x))
                           (recur
                            (+ idx (v y))
                            reg
                            last-sound)

                           (recur
                            (inc idx)
                            reg
                            last-sound))))
                last-sound)))
                (recur))))))

(defn part-2
  [data]
  data)

;; # Tests
{:nextjournal.clerk/visibility {:code   :show
                                :result :hide}}
(deftest test-2017-18
  (testing "part one"
    (is (= 8600 (part-1 input))))

  #_(testing "part two"
      (is (= 1 (part-2 input)))))

{:nextjournal.clerk/visibility {:code   :hide
                                :result :show}}

#_(t/test-render #'test-2017-18)