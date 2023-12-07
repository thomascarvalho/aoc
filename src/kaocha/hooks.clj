(ns kaocha.hooks)

(println "ok")

(defn sample-hook [test test-plan]
  (if (re-find #"fail" (str (:kaocha.testable/id test)))
    (assoc test :kaocha.testable/pending true)
    test))


(defn sample-before-hook [suite test-plan]
  (println "before suite:" (:kaocha.testable/id suite))
  suite)

(defn sample-after-hook [suite test-plan]
  (println "after suite:" (:kaocha.testable/id suite))
  suite)