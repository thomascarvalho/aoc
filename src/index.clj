;; # 🎄 Advent of Code
;;
;; [Advent of Code](https://adventofcode.com) with
;; [Clerk](https://clerk.vision) + Kaocha.
;;
;; Thanks [advent of clerk](https://github.com/nextjournal/advent-of-clerk) and [@elken](https://github.com/elken/) for the template
(ns index
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require
   [babashka.fs :as fs]
   [nextjournal.clerk :as clerk]
   [nextjournal.clerk.view :as clerk.view]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [util :as u]))

(alter-var-root #'clerk.view/include-css+js
                (fn [include-css+js-orig extra-includes]
                  (fn [state]
                    (concat (include-css+js-orig state)
                            extra-includes)))
                (list [:style#extra-styles (slurp (clojure.java.io/resource "style.css"))]))

(defn build-paths []
  (->>
   (-> "src/aoc"
       fs/list-dir
       (fs/list-dirs "*.clj"))
   (sort #(compare %2 %1))))

(defn group-solutions []
  (->> (build-paths)
       (group-by
        (fn [path]
          (let [[_ _ year _] (fs/components path)]
            (str year))))
       (sort-by first #(compare %2 %1))))

{:nextjournal.clerk/visibility {:result :show}}
^::clerk/no-cache
(clerk/html
 (into [:div]
       (mapv (fn [[year paths]]
               [:details
                [:summary.cursor-pointer.w-full
                 [:span.flex-grow.text-2xl.font-bold year]
                 [:span.ml-10 (format "(%s solutions)" (count paths))]]
                (into [:ul.flex.flex-col.gap-1.w-full]
                      (mapv (fn [path]
                              (when-let [day (second (re-find #"(\d+).clj" path))]
                                [:li
                                 [:a {:href (-> path
                                                (str/replace ".clj" "")
                                                clerk/doc-url)}
                                  (u/load-title day year)]]))
                            (map str paths)))])
             (group-solutions))))