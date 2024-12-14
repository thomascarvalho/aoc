^{:nextjournal.clerk/visibility :hide-ns}
(ns test-util
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.test :as test]
            [kaocha.repl :as k]))


{:nextjournal.clerk/visibility {:code   :hide
                                :result :hide}}
(defn test-var? [x]
  (not (nil? (when-let [v (v/get-safe x :nextjournal.clerk/var-from-def)]
               (:test (meta v))))))

(defn with-test-out->str [func]
  (let [s (new java.io.StringWriter)]
    (str (func))))

(defn test-runner-viewer [v]
  (with-test-out->str #(k/run v)))


(def test-viewer {:transform-fn #(-> % :nextjournal/value :nextjournal.clerk/var-from-def test-runner-viewer)
                  :render-fn    '(fn [x]
                                   [:span.syntax-string.inspected-value x])})

(defn init []
  (clerk/add-viewers!
   [{:pred         test-var?
     :transform-fn #(-> % :nextjournal/value :nextjournal.clerk/var-from-def test-runner-viewer)
     :render-fn    '(fn [x]
                      [:span.syntax-string.inspected-value x])}]))

(def run k/run)

(defn test-card [[status nb]]
  [:div {:class (cond-> (str "flex flex-col gap-4 p-6 items-center text-black font-semibold")
                  (= status :pass) (str " bg-green-200")
                  (= status :fail) (str " bg-orange-200")
                  (= status :error) (str " bg-red-200")
                  (zero? nb) (str " opacity-25"))}
   [:span.capitalize.text-lg.text-black (name status)]
   [:span.text-xl.font-bold nb]])

(defn test-render [test-var]
  (clerk/html
   [:div.grid.grid-cols-3.gap-10
    (for [m (select-keys (test/run-test-var test-var) [:pass :fail :error])]
      (test-card m))]))

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
