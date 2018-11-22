(ns day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input
  (-> "day12/input.txt" io/resource io/file slurp))

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
    (->> (range 0 (count graph))
         (map #(set [(hash (set (traverse-graph-dfs graph %)))]))
         (reduce set/union (set nil)))))


(defn part-1 []
  (count (traverse-graph-dfs (parse-input input) 0)))

(defn part-2 []
  (amount-of-groups (parse-input input)))
