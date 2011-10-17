(ns aima.ch3.knuth
  (:use [aima.core.ch3]))

(defn add-factorial-if-not-too-big [state coll]
  (if (< state 170)
    (conj coll (Node. (MathUtils/factorialDouble state) :factorial 1))
    coll))

(defn graph-knuth [n]
  (binding [max-expansions 10000]
    (graph-search
     (Problem. (Node. 4.0 :start 0) n
               (fn [path explored]
                 (let [state (-> path last :state)]
                   (->> [(Node. (Math/sqrt state) :sqrt 1)
                         (Node. (Math/floor state) :floor 1)]
                        (add-factorial-if-not-too-big state)
                        (filter (comp not (partial contains? explored) :state))
                        (map (partial conj path)))))
               count))))
