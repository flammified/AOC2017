(ns day01
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

(def input
  (-> "day01/input.txt"
      io/resource io/file slurp
      (->>
        (str/split-lines)
        (map edn/read-string))))

(defn part-1 []
  (comment "Part 1 goes here"))

(defn part-2 []
  (comment "Part 2 goes here"))
