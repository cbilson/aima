(ns aima.ch3.romania
  (:import [aima.ch3.core]))

(def romanian-roads
  [["Arad" "Zerind" 75]
   ["Zerind" "Oradea" 71]
   ["Oradea" "Sibiu" 151]
   ["Arad" "Sibiu" 140]
   ["Arad" "Timisoara" 118]
   ["Timisoara" "Lugoj" 111]
   ["Lugoj" "Mehadia" 70]
   ["Mehadia" "Drobeta" 75]
   ["Drobeta" "Craiova" 120]
   ["Craiova" "Rimnicu Vilcea" 146]
   ["Craiova" "Pitesti" 138]
   ["Sibiu" "Rimnicu Vilcea" 80]
   ["Sibiu" "Fagaras" 99]
   ["Rimnicu Vilcea" "Pitesti" 97]
   ["Fagaras" "Bucharest" 211]
   ["Pitesti" "Bucharest" 101]
   ["Bucharest" "Giurgiu" 90]
   ["Bucharest" "Urziceni" 85]
   ["Urziceni" "Vaslui" 142]
   ["Urziceni" "Hirsova" 98]
   ["Hirsova" "Eforie" 86]
   ["Vaslui" "Iasi" 92]
   ["Iasi" "Neamt" 87]])

(defn change-edge-direction [start edge]
  (if (= (first edge) start)
    edge
    [(second edge) (first edge) (nth edge 2)]))

(defn expand-romania [path explored]
  (let [from (-> path last :state)
        is-start? (fn [x] (= from (first x)))
        is-end? (fn [x] (= from (second x)))
        is-in? (fn [x] (or (is-start? x) (is-end? x)))]
    (->> (filter is-in? romanian-roads)
         (filter (comp not (partial contains? explored) :state))
         (map (partial change-edge-direction from))
         (map (fn [edge] (conj path (Node. (second edge) :drive (nth edge 2))))))))

(defn distance-to-bucharest [from]
  (condp = from
    "Arad" 366
    "Bucharest" 0
    "Craiova" 160
    "Drobeta" 242
    "Eforie" 161
    "Fagaras" 176
    "Giurgiu" 77
    "Hirsova" 151
    "Iasi" 226
    "Lugoj" 244
    "Mehadia" 241
    "Neamt" 234
    "Oradea" 380
    "Pitesti" 100
    "Rimnicu Vilcea" 193
    "Sibiu" 253
    "Timisoara" 329
    "Urziceni" 80
    "Vaslui" 199
    "Zerind" 374))

(defn a*-romania [start goal]
  (a*-search (Node. start :start 0) goal
             expand-romania
             (fn [path] (reduce + (map :cost path)))
             (comp distance-to-bucharest :state last)))
