(ns util
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [hickory.core :as h]
            [ubergraph.core :as uber]
            [hickory.render :as hr]
            [hickory.select :as s]
            [clojure.pprint :refer [pprint]]
            [babashka.fs :as fs]
            [instaparse.core :as insta]))

(defn to-blocks
  "Turn a blob (probably from `slurp`) into a seq of blocks"
  [input]
  (str/split input #"\n\n"))

(defn to-lines
  "Turn a blob or block into a seq of lines"
  [input]
  (str/split-lines input))

(defn to-matrix
  "Turn a blob (or block) into a vector of vectors"
  ([input]
   (to-matrix identity input))
  ([transform input]
   (->> input
        to-lines
        (mapv #(mapv transform (vec %))))))


(defn vec-remove
  "Remove element at index on a coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn nth-cycling
  "Find any element on a cycling vector"
  [vector i]
  (nth vector (mod i (count vector))))

(defn parse-out-longs
  "Parse out all numbers in `line` that are integers (longs)"
  [line]
  (map parse-long (re-seq #"[-+]?\d+" line)))

;; Like the core time macro, but rather than printing the elapsed time it
;; returns a list of (result, time). Returned value is in milliseconds.
(defmacro time-it [expr]
  `(let [start# (. System (nanoTime))
         ret#   ~expr
         end#   (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     (list ret# end#)))

(defn tee
  "Like 'tap' or 'tee', show the value of expr before returning it"
  [expr]
  (print expr "\n")
  expr)

;; Taken from https://stackoverflow.com/a/3266877/6421
;;
;; Get matches for a given regexp *and* their position within the string.
(defn re-pos
  "Return a list of pairs of (index, string) for all matches of `re` in `s`"
  [re s]
  (loop [m   (re-matcher re s)
         res ()]
    (if (.find m)
      (recur m (cons (list (.start m) (.group m)) res))
      (reverse res))))

(defn re-find-pos
  "Return a list of pairs of (index, string) for all matches of `re` in `s`"
  [re s]
  (let [m   (re-matcher re s)]
    (when (.find m)
      (list (.start m) (.group m)))))

(defn instaparse [data grammar transform]
  (->> data
       ((insta/parser
         grammar))
       #_{:clj-kondo/ignore [:unresolved-var]}
       (insta/transform transform)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn tprint [x]
  (pprint x)
  x)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn spy [d]
  (println d)
  d)

(defn parse-int
  ([s]
   (parse-int s 10))
  ([s b]
   (try (Integer/parseInt s b)
        (catch Exception e nil))))

(defn get-session []
  (or (System/getenv "AOC_TOKEN")
      (let [raw (slurp (str (System/getProperty "user.home") "/.adventofcode.session"))]
        (str/replace raw "\n" ""))))

(defn load-problem
  "Given a DAY and a YEAR, cache the problem definition locally. If `AOC_TOKEN`
  is set correctly, this will pull both parts if you've done part 1."
  [day year]
  (let [day       (str (parse-long day))
        file-name (format "day%s-%s.html" day year)
        path      (fs/path (fs/temp-dir) file-name)]
    (when-not (fs/exists? path)
      (let [resp (->> {:headers {"Cookie" (str "session=" (get-session))
                                 }}
                      (client/get (format "https://adventofcode.com/%s/day/%s" year day)))]
        (when (= 200 (:status resp))
          (spit (str path) (:body resp)))))

    (let [doc   (h/as-hickory (h/parse (slurp (str path))))
          parts (map #(hr/hickory-to-html %) (s/select (s/child (s/tag :article)) doc))]
      (apply str (mapcat str parts)))))

(defn load-title
  "Given a DAY and a YEAR, return the title of the problem."
  [day year]
  (let [day       (str (parse-long day))
        file-name (format "day%s-%s.html" day year)
        _         (load-problem day year)
        path      (fs/path (fs/temp-dir) file-name)
        doc       (h/as-hickory (h/parse (slurp (str path))))
        parts     (map #(hr/hickory-to-html %) (s/select (s/child (s/tag :h2)) doc))]
    (->> parts
         first
         (re-find #"(?<=>--- )(.*)(?= ---<)")
         first)))

(defn absolute-distance [s1 s2]
  (Math/abs (- s1 s2)))

(defn manhattan-distance [v1 v2]
  (->> (map absolute-distance v1 v2)
       (reduce +)))

(defn find-all-paths
  [graph start end]
  (letfn [(find-paths [node path]
            (if (= node end)
              [path]
              (apply concat (map #(find-paths %1 (conj path %1))
                                 (remove #(contains? (set path) %) (uber/successors graph node))))))]
    (find-paths start [start])))

(defn assoc-at [data i item]
  (if (associative? data)
    (assoc data i item)
    (if-not (neg? i)
      (letfn [(assoc-lazy [i data]
                (cond (zero? i) (cons item (rest data))
                      (empty? data) data
                      :else (lazy-seq (cons (first data)
                                            (assoc-lazy (dec i) (rest data))))))]
        (assoc-lazy i data))
      data)))