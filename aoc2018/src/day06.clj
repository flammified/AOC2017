(ns day06
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(def text
  (-> "day06/input.txt"
      io/resource
      io/file
      slurp
      str/split-lines))

; hmmmm also slow af

(def input
  (->> text
       (mapv #(re-seq #"\d+" %))
       (mapv #(mapv edn/read-string %))))

(def coord-to-letters (merge (zipmap input (map char (range 65 91))) {nil "X"}))

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (math/abs (- x2 x1)) (math/abs (- y2 y1))))

(defn belongs-to [nodes point]
  (let [distances (map vector nodes (map (partial manhattan point) nodes))
        smallest-distance (second (apply min-key second distances))
        points-with-smallest-distance (filter #(= (second %) smallest-distance) distances)]
    (if (= (count points-with-smallest-distance) 1) (ffirst points-with-smallest-distance) nil)))

(defn all-points [[max-x max-y]]
  (for [y (range 0 max-y)
        x (range 0 max-x)]
    [x y]))

(defn aabb [nodes]
  (let [max-x (apply max (map first nodes))
        max-y (apply max (map second nodes))]
    [max-x max-y]))

(defn nodes-touching-edges [nodes grid [max-x max-y]]
  (let [edges (filter (fn [[x y]] (or (= x 0) (= x max-x) (= y 0) (= y max-y))) grid)]
    (into #{} (map (partial belongs-to nodes) edges))))

(defn biggest-finite-area [nodes]
  (let [bounding-box (aabb nodes)
        grid (time (all-points bounding-box))
        edge-nodes (time (nodes-touching-edges nodes grid bounding-box))
        claimed-areas (time (map (partial belongs-to nodes) grid))
        points-in-finite-areas (time (filter #(nil? (edge-nodes %)) claimed-areas))]
    (second (apply max-key second (time (frequencies points-in-finite-areas))))))



(defn part-1 []
  (biggest-finite-area input))

(defn part-2 []
  (count (filter (fn [p] (< (reduce + (map (partial manhattan p) input)) 10000)) (all-points [500 500]))))
