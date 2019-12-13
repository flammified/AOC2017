(ns day06
  (:require [clojure.string :as str]
            [clojure.java.io :as io])

  (:use [loom.graph]
        [loom.alg]))

(def input
  (-> "day06/input.txt"
      io/resource io/file slurp str/trim
      (str/split-lines)
      (->> (map #(str/split % #"\)")) (map #(concat %[1])) (apply weighted-graph))))

(defn part-1 []
  (reduce + (map first (-> (dijkstra-traverse input "COM") last second vals))))

(defn part-2 []
  (- (second (dijkstra-path-dist input "SAN" "YOU")) 2))
