(ns day05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.async :refer [>!! <!! to-chan chan]]
            [util.intcode :refer [run-sync]]))

(def input
  (-> "day05/input.txt"
      io/resource io/file slurp str/trim
      (str/split #",")
      (->> (map edn/read-string) (into []))))

(defn part-1 []
  (last (:output (run-sync input [1]))))

(defn part-2 []
  (last (:output (run-sync input [5]))))
