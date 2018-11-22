(ns day12.core
  (:use [clojure.string :as str])
  (:require [clojure.set :as set]))


(def input (slurp "input.txt"))

(defn parse-line [state line]
  (let [[id connections-str] (str/split line #" <-> ")
        connections (vec (map read-string (str/split connections-str #",")))]
    (conj state connections)))

(defn parse-input [text]
  (reduce parse-line [] (str/split-lines text)))

(defn seq-contains? [coll target] (some #(= target %) coll))

(defn traverse-graph-dfs [g s]
  (loop [vertices [] explored #{s} frontier [s]]
    (if (empty? frontier)
      vertices
      (let [v (peek frontier)
            neighbors (g v)]
        (recur
          (conj vertices v)
          (into explored neighbors)
          (into (pop frontier) (remove explored neighbors)))))))

(defn amount-of-groups [graph]
  (count
    (reduce
      set/union
      (set nil)
      (map
        #(set [(hash (set (traverse-graph-dfs graph %)))])
        (range 0 (count graph))))))

(defn -main []
  (println (amount-of-groups (parse-input input))))
