(ns day06
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as s]
            [clojure.data.priority-map :refer [priority-map]])

  (:use [loom.graph]
        [loom.alg]))

(def input
  (-> "day06/input.txt"
      io/resource io/file slurp str/trim
      (str/split-lines)
      (->> (map #(str/split % #"\)")) (map #(concat %[1])) (apply weighted-graph))))

(defn solve-part-1 [input]
  (reduce + (map (comp second (partial dijkstra-path-dist input "COM")) (nodes input))))

(defn solve-part-2 [input]
  (- (second (dijkstra-path-dist input "SAN" "YOU")) 2))

(defn part-1 []
  (solve-part-1 input))

(defn part-2 []
  (solve-part-2 input))

(println (part-2))
