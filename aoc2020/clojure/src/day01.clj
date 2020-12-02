(ns day01
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as c]))

(def input
  (-> "day01/input.txt"
      io/resource io/file slurp
      (->>
        (str/split-lines)
        (map edn/read-string))))

(defn part-1 []
  (->> (c/combinations input 2)
      (filter (fn [p] (= (apply + p) 2020)))
      (map #(apply * %))
      (first)))

(defn part-2 []
  (->> (c/combinations input 3)
      (filter (fn [p] (= (apply + p) 2020)))
      (map #(apply * %))
      (first)))

(part-2)
