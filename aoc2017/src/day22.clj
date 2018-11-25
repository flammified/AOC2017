(ns day22
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.core.matrix :as matrix]))

(def input
  (-> "day22/input.txt" io/resource io/file slurp))

(defn input-to-grid [input]
    (->> input
      (str/split-lines)
      (mapv #(into [] (str/split % #"")))
      (map-indexed
        (fn [idx-y row]
          (reduce-kv (fn [res idx-x char] (merge res (if (= char "#") {[idx-y idx-x] :infected}))) {} row)))
      (into {})))

(defn move [state]
  (apply update-in state
    (case (:direction state)
      :down  [[:position 0] inc]
      :up    [[:position 0] dec]
      :left  [[:position 1] dec]
      :right [[:position 1] inc])))


(def state-cycle
  {:clean :weakened
   :weakened :infected
   :infected :flagged
   :flagged :clean})


(defn burst [{:keys [position direction grid] :as state}]
  (let [current-node (grid position :clean)
        [dy dx] direction
        new-direction (case current-node
                            (:clean) [(- dx) dy]
                            :weakened [dy dx]
                            :infected [dx (- dy)]
                            :flagged [(- dy) (- dx)])]

    (-> state
      (update :grid assoc position (state-cycle current-node))
      (assoc :direction new-direction
             :position (mapv + position new-direction))
      (cond-> (= :weakened current-node) (update :count inc)))))

(defn part-2 []
  (time (:count (last (take 10000000 (iterate burst {:count 0 :position [12 12] :direction [-1 0] :grid (input-to-grid input)}))))))
