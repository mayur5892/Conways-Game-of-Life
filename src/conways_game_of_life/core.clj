(ns conways-game-of-life.core)

(defn calculate-neighbours-coordinates [point]
  (let [x (first point)
        y (second point)]
    #{[(+ x 1) y]
      [(- x 1) y]
      [x (+ y 1)]
      [x (- y 1)]
      [(+ x 1) (+ y 1)]
      [(+ x 1) (- y 1)]
      [(- x 1) (+ y 1)]
      [(- x 1) (- y 1)]}))

(defn find-all-neighbours [point current-generation]
  (map (fn [neighbour-coordinate]
         {:coordinate neighbour-coordinate
          :state (if (current-generation neighbour-coordinate)
                   true
                   false)})
       (calculate-neighbours-coordinates point)))

(defn compute-cell-next-gen-state [cell live-cells]
  (println "computing for " cell)
  (let [neighbours (find-all-neighbours (:coordinate cell) live-cells)
        live-neighbours (count (filter #(:state %) neighbours))
        currently-active? (:state cell)]
    (if currently-active?
      (if (or (= live-neighbours 3) (= live-neighbours 2))
        cell
        (assoc cell :state false))
      (if (= live-neighbours 3)
        (assoc cell :state true)
        cell))))

(defn extract-live-cell [cells]
  (->> cells
       (filter #(:state %))
       (map :coordinate)
       (into #{})))

(defn find-dead-neighbours [cell current-live-cells]
  (->> (find-all-neighbours cell current-live-cells)
       (remove #(:state %))))

(defn better-name [state cell]
  (clojure.pprint/pprint state)
  (let [dead-neighbours (remove #((:already-processed state) (:coordinate %)) (find-dead-neighbours (:coordinate cell) (:current-live-cells state)))
        remain-alive-for-next-gen (->> (conj dead-neighbours cell)
                                       (map #(compute-cell-next-gen-state % (:current-live-cells state)))
                                       (filter #(:state %)))]
    (-> state
        (update :remain-alive-for-next-gen concat remain-alive-for-next-gen)
        (update :already-processed (fn [value new-value]
                                     (->> value
                                         (concat new-value)
                                         (into #{}))) ( map :coordinate dead-neighbours)))))

(defn compute-future-generation [present-generation number-of-generation]
  (println "input " present-generation)
  (let [live-cells (extract-live-cell present-generation)
        ;;remain-alive-for-next-gen (filter #(is-alive-in-next-gen? % live-cells) present-generation)
        ]
    (:remain-alive-for-next-gen (reduce better-name
            {:remain-alive-for-next-gen (seq [])
             :already-processed         #{}
             :current-live-cells live-cells}
            present-generation))))

(def cgen #{
            ;;{:coordinate [1 2], :state true}
            {:coordinate [2 1], :state true}
            ;;{:coordinate [2 0], :state true}
            {:coordinate [1 0], :state true}})

(compute-future-generation cgen 1)