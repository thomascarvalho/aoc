(ns aoc.2024.21
  {:nextjournal.clerk/toc true}
  (:require [clojure.java.io :as io]
            [nextjournal.clerk :as clerk]
            [util :as u]
            [pathfinding :as pf]
            [ubergraph.alg :as alg]
            [clojure.set :as set]
            [ubergraph.core :as uber]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn decode-code [code-str]
  (mapv (fn [c]
          (or (u/parse-int c) (str c)))
        (str/split code-str #"")))


;; # Parser
(defn parser [data]
  (->> data
       u/to-lines
       (mapv (fn [raw]
               [(first (u/parse-out-longs raw))
                (decode-code raw)]))))

;; Inputs
(def input (->> (slurp (io/resource "inputs/2024/21.txt"))
                parser))

(def input-example (parser "029A
980A
179A
456A
379A"))

;; Logic

; Numeric Keypad
; +---+---+---+
; | 7 | 8 | 9 |
; +---+---+---+
; | 4 | 5 | 6 |
; +---+---+---+
; | 1 | 2 | 3 |
; +---+---+---+
;     | 0 | A |
;     +---+---+

(def numeric-keypad-edges [["A" 3 {:dir "^"}]
                           ["A" 0 {:dir "<"}]
                           [0 "A" {:dir ">"}]
                           [3 "A" {:dir "v"}]
                           [0 2 {:dir "^"}]
                           [1 4 {:dir "^"}]
                           [1 2 {:dir ">"}]
                           [2 1 {:dir "<"}]
                           [2 5 {:dir "^"}]
                           [2 3 {:dir ">"}]
                           [3 2 {:dir "<"}]
                           [3 6 {:dir "^"}]
                           [4 1 {:dir "v"}]
                           [4 7 {:dir "^"}]
                           [4 5 {:dir ">"}]
                           [5 2 {:dir "v"}]
                           [5 4 {:dir "<"}]
                           [5 8 {:dir "^"}]
                           [5 6 {:dir ">"}]
                           [6 3 {:dir "v"}]
                           [6 5 {:dir "<"}]
                           [6 9 {:dir "^"}]
                           [7 8 {:dir ">"}]
                           [7 4 {:dir "v"}]
                           [8 7 {:dir "<"}]
                           [8 5 {:dir "v"}]
                           [8 9 {:dir ">"}]
                           [9 8 {:dir "<"}]
                           [9 6 {:dir "v"}]])
;     +---+---+
;     | ^ | A |
; +---+---+---+
; | < | v | > |
; +---+---+---+
(def directions-keypad-edges [["^" "A" {:dir ">"}]
                              ["^" "v" {:dir "v"}]
                              ["A" "^" {:dir "<"}]
                              ["A" ">" {:dir "v"}]
                              ["<" "v" {:dir ">"}]
                              ["v" "<" {:dir "<"}]
                              ["v" "^" {:dir "^"}]
                              ["v" ">" {:dir ">"}]
                              [">" "A" {:dir "^"}]
                              [">" "v" {:dir "<"}]])

(defn make-directed-graph
  "Crée un graphe orienté à partir d'une liste d'arêtes avec attributs
   Chaque arête est de la forme [from to attributes]"
  [edges]
  (reduce (fn [g [v1 v2 attrs]]
            (update g v1 (fnil conj #{}) [v2 attrs]))
          {}
          edges))

(def numeric-keypad-graph (make-directed-graph numeric-keypad-edges))
(def directions-keypad-graph (make-directed-graph directions-keypad-edges))

(defn reconstruct-path-with-attrs
  "Reconstruit le chemin avec les attributs à partir de la map des parents"
  [parents start end]
  (loop [current end
         path-with-attrs []]
    (if (nil? current)
      ""
      (let [parent-info (get parents current)
            new-path (if parent-info
                       (cons (get-in parent-info [1 :dir]) path-with-attrs)
                       path-with-attrs)]
        (if (= (first parent-info) start)
          (str/join new-path)
          (recur (first parent-info) new-path))))))

(defn shortest-path
  "Trouve le plus court chemin en utilisant une file prioritaire"
  [graph start end]
  (loop [queue (priority-map start 0)  ; File prioritaire
         visited #{}
         distances {start 0}
         parents {}]
    (if (empty? queue)
      nil
      (let [current (first (peek queue))  ; Nœud avec la plus petite distance
            current-dist (get distances current)
            queue (pop queue)]
        (if (= current end)
          (reconstruct-path-with-attrs parents start end)
          (if (visited current)
            (recur queue visited distances parents)
            (let [neighbors (get graph current #{})
                  updates (for [[node attrs] neighbors
                                :let [new-dist (inc current-dist)]
                                :when (or (not (contains? distances node))
                                          (< new-dist (get distances node)))]
                            [node [new-dist [current attrs]]])
                  new-queue (into queue (map (fn [[node [dist _]]] [node dist]) updates))
                  new-distances (into distances (map (fn [[node [dist _]]] [node dist]) updates))
                  new-parents (into parents (map (fn [[node [_ parent]]] [node parent]) updates))]
              (recur new-queue
                     (conj visited current)
                     new-distances
                     new-parents))))))))

(defn dijkstra
  "Trouve le plus court chemin selon l'algorithme de Dijkstra"
  [graph start end]
  (loop [queue (priority-map start 0)         ; File prioritaire avec distances
         visited #{}                          ; Nœuds définitivement étiquetés
         distances {start 0}                  ; Distances depuis le départ
         parents {}]                          ; Pour reconstruire le chemin
    (if (empty? queue)
      nil  ; Pas de chemin trouvé
      (let [current (first (peek queue))      ; Nœud avec plus petite distance
            current-dist (get distances current)
            queue (pop queue)]
        (if (= current end)
          (reconstruct-path-with-attrs parents start end)
          (if (visited current)
            (recur queue visited distances parents)
            (let [neighbors (get graph current #{})
                  updates (for [[node attrs] neighbors
                                :let [new-dist (+ current-dist 1)]  ; Distance = 1
                                :when (or (not (contains? distances node))
                                          (< new-dist (get distances node)))]
                            [node [new-dist [current attrs]]])
                  new-queue (into queue (map (fn [[node [dist _]]] [node dist]) updates))
                  new-distances (into distances (map (fn [[node [dist _]]] [node dist]) updates))
                  new-parents (into parents (map (fn [[node [_ parent]]] [node parent]) updates))]
              (recur new-queue
                     (conj visited current)
                     new-distances
                     new-parents))))))))

(defn join-A [code]
  (str (str/join "A" code) "A"))

(defn path-code [graph code]
  (loop [moves code
         from "A"
         path []]
    (if-let [to (first moves)]
      (recur (next moves)
             to
             (conj path (dijkstra graph from to)))
      (decode-code (join-A path)))))

;; ## Part 1
(defn part-1
  [codes]
  (->> (for [[numeric-code code] codes
             :let [k1 (path-code numeric-keypad-graph code)
                   k2 (path-code directions-keypad-graph k1)
                   k3 (path-code directions-keypad-graph k2)]]
         [numeric-code (count k3)])
       #_#_(map #(apply * %))
         (reduce +)))

; <v<A>^>A<A>AvA<^AA>A<vAAA^>A
; v<<A>>^A<A>AvA<^AA>A<vAAA>^A

; <v<A>A<A>^>AvA<^Av>A^A<v<A>^>AvA^A<vA^>A<v<A>^A>AAvA^A<v<A>A^>AAA<Av>A^A
; <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A

;; to high 206362

;; ## Part 2
#_(defn part-2
    [data]
    data)

;; # Tests
#_{:nextjournal.clerk/visibility {:code   :show
                                  :result :hide}}
#_(deftest test-2024-21
    #_(testing "part one"
        (is (= 1 (part-1 input))))

    #_(testing "part two"
        (is (= 1 (part-2 input)))))

