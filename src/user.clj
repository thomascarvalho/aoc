(ns user)

(defn plus [a b]
  (+ a b))

(defn- plus-one [a]
  (plus a 1))

(defn minus [a b]
  (- a b))

(def data {:name "Thomas"
           :age 4})

(-> 1
    (plus 2)
    plus-one)

(comment)
