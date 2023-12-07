(ns aoc.2022.07
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [clojure.string :as cs]
            [util :refer [read-from-ns parse-int]]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

(def input (read-from-ns ::x))

(def input-example "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn parser [i]
  (->> i
       ((insta/parser
         "<RESULT> = COMMAND*
       <COMMAND> = <'$ '> ( LS | CD )*
       DIR = <'dir '> DIRNAME <NEWLINE>?
       FILE = SIZE <' '> FILENAME <NEWLINE>?
       LS = <'ls'> <NEWLINE> (FILE | DIR)*
       CD = 'cd ' (BACK | DIRNAME | ROOT) <NEWLINE>
       BACK = '..'
       ROOT = '/'   
       DIRNAME = #'\\w+'
       <FILENAME> = #'\\w+(\\.\\w+)?'
       SIZE = #'\\d+'
       <SPACE> = ' '
       <NEWLINE> = #'\\n'
          "))
       #_{:clj-kondo/ignore [:unresolved-var]}
       (insta/transform {:DIRNAME str
                         :ROOT    (fn [x] "ROOT")
                         :SIZE    (fn [x] (parse-int x))
                         :BACK    (fn [x] :BACK)
                         :CD      (fn [k r] [:CD r])})))


(defn assoc-graph [state graph]
  (swap! state assoc :graph graph))

(defmulti process-command (fn [[command] _] command))

(defmethod process-command :LS [[_ & items] state]
  (doseq [item items]
    (let [g                               (:graph @state)
          [current-dir current-dir-attrs] (uber/node-with-attrs g (:current-dir @state))]
      (case (first item)
        :DIR (let [dir-name (second item)]
               (assoc-graph state
                            (-> g
                                (uber/add-nodes-with-attrs [dir-name {:type :dir}])
                                (uber/add-directed-edges [current-dir dir-name]))))
        :FILE (assoc-graph state
                           (let [filename  (nth item 2)
                                 file-size (second item)]
                             (-> g
                                 (uber/add-nodes-with-attrs [filename {:type :file
                                                                       :size file-size}])
                                 (uber/add-directed-edges [current-dir filename]))))))))

(defmethod process-command :CD [[_ dir] state]
  (if (= dir :BACK)
    (let [h           (:history @state)
          new-history (butlast h)
          new-dir     (take-last 1 h)]
      (swap! state assoc :current-dir new-dir :history new-history))
    (do
      (when (:current-dir @state) (swap! state update :history conj (:current-dir @state)))
      (swap! state assoc :current-dir dir))))

(defn create-initial-graph [commands]
  (let [state (atom {:current-dir nil
                     :history     []
                     :graph       (uber/graph ["ROOT" {:total 0}])})]
    (doseq [command commands]
      (process-command command state))
    (:graph @state)))

(defn assoc-total-on-graph-directories [initial-graph]
  (let [dirs  (reverse (alg/bf-span initial-graph "ROOT"))
        state (atom {:graph initial-graph})]
    (doseq [[dir children] dirs]
      (let [g         (:graph @state)
            [_ attrs] (uber/node-with-attrs g dir)
            total     (reduce + (map (fn [c]
                                       (let [[_ {:keys [type total size]}] (uber/node-with-attrs g c)]
                                         (if (= type :file)
                                           size
                                           (or total 0)))) children))]
        (assoc-graph state (uber/set-attrs g dir  (assoc attrs :total total)))))

    (:graph @state)))

(defn part-one
  ([]
   (part-one input))
  ([input]
   (let [commands      (parser input)
         initial-graph (create-initial-graph commands)
         g             (assoc-total-on-graph-directories initial-graph)]
     #_(uber/pprint g)
     (->>
      (uber/nodes g)
      (map #(let [[_ {:keys [total]}] (uber/node-with-attrs g %)]
              (if (and total (<= total 100000))
                total
                0)))
      (reduce +))

     #_(uber/pprint g)
     ;
     )))

(defn part-two []
  (let [data input]
    0))

(deftest test-2022-07

  (testing "part one - example"
    (is (= 95437 (part-one input-example))))

  #_(testing "part one"
    (is (= 1491614 (part-one))))

  #_(testing "part two"
      (is (= 6400111 (part-two)))))





(part-one input-example)