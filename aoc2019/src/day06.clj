(ns day06
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as s]
            [clojure.data.priority-map :refer [priority-map]]))

(defn children [a]
  (reduce
    (fn [ res [a b :as orbit]]
      (update res a (fnil conj []) b))
    {}
    a))

(defn parents [a]
  (reduce
    (fn [res [a b :as orbit]]
      (update res b (fnil conj []) a))
    {}
    a))

(defn planets [a]
  (s/union (set (keys a)) (reduce #(s/union %1 %2) #{} (vals a))))

(defn find [all right val]
  (first (filter (fn [p] (contains? (set (get right p)) val )) all)))

(def input
  (-> "day06/input.txt"
      io/resource io/file slurp str/trim
      (str/split-lines)
      (->> (map #(str/split % #"\)" )) (mapv #(map str %)))))

(defn bfs [parents children stack planet]
  (if (some? (get children planet))
    (reduce + (count stack) (map (partial bfs parents children (conj stack planet)) (get children planet)))
    (count stack)))

(defn adjacency-map [parents children planet]
  (zipmap (concat (get children planet) (get parents planet)) (repeat 1)))

;; Taken from https://gist.github.com/myfreeweb/1175566

(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.
   Given a node n, (f n) should return a map with the successors of n as keys
   and their (non-negative) distance from n as vals.
   Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn solve-part-1 [input]
  (let [children (children input)
        parents (parents input)]
      (bfs parents children [] "COM")))

(defn solve-part-2 [input]
  (let [children (children input)
        parents (parents input)]
    (let [all-planets (planets children)
          san-planet (find all-planets children "SAN")
          you-planet (find all-planets children "YOU")]
      (get (dijkstra san-planet (partial adjacency-map parents children)) you-planet))))


(defn part-1 []
  (solve-part-1 input))

(defn part-2 []
  (solve-part-2 input))
