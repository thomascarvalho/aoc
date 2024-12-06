(ns every.13
  (:require [util :as u]
            [medley.core :as m]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [pathfinding :as pf]))

(def input-exemple "#######
#6769##
S50505E
#97434#
#######")

(def input "#S##  ##########  ##########
#34#  #90713919#  #00906738#
#59#  #70273329#  #30042505#
#92#  #54####71#  #43####80#
#76#  #43#  #22#  #88#  #61#
#28####14#  #07####01#  #07#
#58841470#  #87390910#  #76#
#76216946#  #95129392#  #92E
##########  ##########  ####")

(defn parser [data]
  (->> data
       u/to-matrix
       pf/decode-matrix))

(defn get-value [v]
  (or (u/parse-int (str v)) 0))

(let [{:keys [cells]} (parser input)
      real-cells (->> cells
                      (m/filter-vals (fn [v] (and (not= v \#) (not= v \space)))))
      start  (ffirst (m/filter-vals (fn [v] (= v \S)) real-cells)) 
      end (ffirst (m/filter-vals (fn [v] (= v \E)) real-cells))
      edges (->> real-cells
                 (mapcat (fn [[k v]]
                           (let [v1 (get-value v)]
                             (mapv
                              (fn [k2]
                                (let [v2 (get-value (get real-cells k2))]
                                  [k k2 {:weight (inc (abs (- v1 v2)))}]))
                              (pf/neighbors k real-cells))))))
      g (-> (uber/digraph)
            (uber/add-edges* edges))
      shortest-path (alg/shortest-path g {:start-node start 
                                          :end-node end 
                                          :cost-fn (fn [{:keys [src dest]}] 
                                                      (inc (abs (- (get-value (get real-cells dest)) (get-value (get real-cells src)))))) 
                                                     
                                          #_#_:cost-attr :weight})] 
    (:cost shortest-path))

           
  

