;; # 🎄 Advent of Code
;;
;; My solutions for [Advent of Code](https://adventofcode.com) in **clojure** with [Clerk](https://clerk.vision).
;;
;; Repository adapted from [advent of clerk](https://github.com/nextjournal/advent-of-clerk) and [@elken](https://github.com/elken/)'s template
(ns index
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require
   [babashka.fs :as fs]
   [nextjournal.clerk :as clerk]
   [nextjournal.clerk.view :as clerk.view]
   [clojure.java.io :as io]
   [aoc.2023.index]
   [aoc.2015.index]
   [aoc.2016.index]
   [aoc.2017.index]
   [aoc.2018.index]
   [aoc.2019.index]
   [aoc.2020.index]
   [aoc.2024.index]
   [aoc.2021.index]
   [aoc.2022.index]))

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
       (fs/list-dirs "index.clj"))
   (sort #(compare %2 %1))))

(defn group-solutions []
  (->> (build-paths)
       (group-by
        (fn [path]
          (let [[_ _ year _] (fs/components path)]
            (str year))))
       (sort-by first #(compare %2 %1))))

(defn get-days-for-year [year]
  (let [ns-symbol (symbol (str "aoc." year ".index"))
        publics   (ns-publics ns-symbol)
        days      (publics (symbol "days"))]
    (days)))


{:nextjournal.clerk/visibility {:result :show}}
^::clerk/no-cache
(clerk/html
 (into [:div.grid.grid-cols-4.gap-10]
       (mapv (fn [[year paths]]
               (let [completed-days (get-days-for-year year)]
                 [:a {:class "font-display block flex flex-col !text-[#331832] hover:no-underline border hover:border-[#127475] gap-6 items-center p-6 bg-[#C6D8D3] rounded-sm hover:shadow-md cursor-pointer"
                      :href  (clerk/doc-url (str "src/aoc/" year "/index"))}
                  [:span.text-2xl.font-bold year]
                  [:span.text-right.text-lg (format "%s/25" (count (filter (fn [[_ {:keys [stars]}]]
                                                                             (= stars 2)) completed-days)))]
                  [:span {:class "text-right font-bold text-xl text-yellow-500"} (format "%s*" (apply + (map :stars (vals completed-days))))]]))
             (group-solutions))))
