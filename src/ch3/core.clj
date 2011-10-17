(ns aima.ch3.core
  (:import [org.apache.commons.math.util MathUtils]
           [java.lang Math]))

(def ^:dynamic max-expansions 100)

(defrecord Node [state action cost])

(defrecord Problem [initial-state goal expand cost])

(defn goal? [problem path]
  (= (:goal problem) (:state (last path))))

(defn print-path [ordinal path]
  (println ordinal ") "
           (apply str (interpose " -> " (map (fn [n] (str (:action n) " to " (:state n))) path)))))

(defn insert-path [problem paths path]
  (let [g (:cost problem)
        gx (g path)
        split-fn (fn [y] (>= gx (g y)))]
    (concat (take-while split-fn paths)
            (list path)
            (drop-while split-fn paths))))

(defn insert-paths [problem existing-paths new-paths]
  (reduce (partial insert-path problem) existing-paths new-paths))

(defn tree-search [problem]
  (loop [frontier (list [(:initial-state problem)])
         recur-count 0]
    (if-let [path (first frontier)]
      (do
        (print-path recur-count path)
        (if (goal? problem path)
          path
          (if (< recur-count max-expansions)
            (recur (->> ((:expand problem) path #{})
                        (insert-paths problem (rest frontier)))
                   (inc recur-count))))))))

(defn graph-search [problem]
  (loop [frontier (list [(:initial-state problem)])
         explored #{}
         recur-count 0]
    (if-let [path (first frontier)]
      (do
        (print-path recur-count path)
        (cond
         (goal? problem path) path
         (< recur-count max-expansions)
         (let [leaf (-> path last :state)]
           (recur (->> ((:expand problem) path explored)
                       (insert-paths problem (rest frontier)))
                  (conj explored leaf)
                  (inc recur-count))))))))

(defn a*-search [start goal expansion-fn g h]
  (-> (Problem. start goal
                expansion-fn
                (fn [x] (+ (g x) (h x))))
      graph-search))
