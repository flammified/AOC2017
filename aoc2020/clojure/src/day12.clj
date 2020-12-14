(ns day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [util.grids :as grids]))



(defn parse [l]
  (let [d (cond (str/includes? l "F") :forward
                (str/includes? l "N") :north
                (str/includes? l "E") :east
                (str/includes? l "W") :west
                (str/includes? l "S") :south
                (str/includes? l "B") :backward
                (str/includes? l "L") :left
                (str/includes? l "R") :right)
        val (first (map edn/read-string (re-seq #"-?\d+" l)))]

    {:action d :length val}))

(def input
  (-> "day12/input.txt"
      io/resource
      io/file slurp
      str/trim
      str/split-lines
      (->> (map parse))))

(defn calculate-rotate [dir l amount]
  (loop [dir dir
         amount amount]
    (if (<= amount 0)
      dir
      (recur (grids/rotate l dir) (- amount 90)))))

(defn step-x [pos dir amount]
  (reduce
    (fn [pos _]
      (grids/step pos dir))
    pos
    (range amount)))

(defn part-1 []
  (->>
    (reduce
      (fn [{:keys [pos dir]} {:keys [action length]}]
        (case action
          :left {:pos pos
                 :dir (calculate-rotate dir action length)}
          :right {:pos pos
                  :dir (calculate-rotate dir action length)}
          :forward {:pos (step-x pos dir length)
                    :dir dir}
          {:pos (step-x pos action length)
           :dir dir}))

      {:pos [0 0] :dir :east}
      input)
    :pos
    (grids/manhattan [0 0])))

(defn rotate-waypoint [way [n e] deg]
  (-> (iterate
        (fn [[n e]]
          (case way
            :left [e (- 0 n)]
            :right [(- 0 e) n]))
        [n e])
      (nth (int (/ deg 90)))))


(defn part-2 []
  (->>
    (reduce
      (fn [{:keys [waypoint pos]} {:keys [action length]}]
        (case action
          :left {:waypoint (rotate-waypoint action waypoint length)
                 :pos pos}
          :right {:waypoint (rotate-waypoint action waypoint length)
                  :pos pos}
          :forward {:waypoint waypoint
                    :pos (nth (iterate #(mapv + % waypoint) pos) length)}
          {:pos pos
           :waypoint (step-x waypoint action length)}))

      {:pos [0 0] :waypoint [10 -1]}
      input)
    :pos
    (grids/manhattan [0 0])))
