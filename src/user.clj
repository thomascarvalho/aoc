(ns user
  (:require [emmy.clerk :as ec]))

;; start Clerk's built-in webserver on the default port 7777, opening the
;; browser when done
#_(ec/serve! {:browse      true
            :index       "src/index.clj"
            :compile-css true
            :watch-paths ["src"]
            :paths       [;;   "src/aoc/2015/*.clj"
                        ;;   "src/aoc/2018/*.clj"
                        ;;   "src/aoc/2019/*.clj"
                        ;;   "src/aoc/2020/*.clj"
                        ;;   "src/aoc/2021/*.clj"
                                                                     ;; "src/aoc/2022/*.clj" ;; FIXME: clerk build crashes with 2022 => find why 
                          "src/aoc/2023/*.clj"
                          "src/*.clj"]})