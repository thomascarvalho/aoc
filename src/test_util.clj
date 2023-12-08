^{:nextjournal.clerk/visibility :hide-ns}
(ns test-util
  (:require [nextjournal.clerk :as clerk]
            [kaocha.repl :as k]))

(def run k/run)

(defn render-results [results]
  (let [r results]
    (clerk/html
     [:div
      (clerk/table {:nextjournal.clerk/width :prose}
                   {:head (map name (keys r))
                    :rows [(vals r)]})])))

(defn render-html [results]
  (clerk/html
   (into
    [:div
     [:h2 "Test results"]
     [:ul.w-full.flex]]
    (for [[k v] results
          :when (and (not= :kaocha.result/count k) (pos? v))]
      [:li.flex
       [:span.capitalize.flex-grow k]
       [:span.inline-flex.gap-2 v
        #_(repeat v [:svg.text-green-600
                   {:xmlns        "http://www.w3.org/2000/svg"
                    :fill         "none"
                    :viewBox      "0 0 24 24"
                    :stroke-width "1.5"
                    :stroke       "currentColor"
                    :class        "w-6 h-6"}
                   [:path
                    {:stroke-linecap  "round"
                     :stroke-linejoin "round"
                     :d               "M9 12.75L11.25 15 15 9.75M21 12a9 9 0 11-18 0 9 9 0 0118 0z"}]])]]))))