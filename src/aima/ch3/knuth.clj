(ns aima.ch3.knuth
  (:use [aima.ch3.core])
  (:import [org.apache.commons.math.util MathUtils]
           [java.lang Math])
  (:gen-class))

(defn add-factorial-if-not-too-big [state coll]
  (if (< state 170)
    (conj coll (node (MathUtils/factorialDouble state) :factorial 1))
    coll))

(defn search-knuth-conjecture [n]
  (binding [max-expansions 100000]
    (graph-search
     (problem (node 4.0 :start 0)
              (fn [x] (= (Math/floor x) n))
              (fn [path explored]
                (let [state (-> path last :state)]
                  (->> [(node (Math/sqrt state) :sqrt 1)
                        (node (Math/floor state) :floor 1)]
                       (add-factorial-if-not-too-big state)
                       (filter (comp not (partial contains? explored) :state))
                       (map (partial conj path)))))
              count))))

(defn -main [n]
  (let [answer (search-knuth-conjecture n)]
    (if answer
      (do
        (println "Answer found:")
        (println "Start with 4.0")
        (doseq [step (rest answer)]
          (println "then " (-> step :action name) " to get " (:state step))))
      (println "Answer not found"))))
