(ns day06
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def input
  (-> "day06/input.txt"
      io/resource
      io/file slurp
      str/trim))


(defn part-1 []
  (reduce
    (fn [total group]
      (+ total
        (count
          (apply set/union
            (map
              (fn [l]
                (set (str/split l #"")))
              (str/split-lines group))))))
    0
    (str/split input #"\n\n")))


(defn part-2 []
  (reduce
    (fn [total group]
      (+ total
        (count
          (apply set/intersection
            (map
              (fn [l]
                (set (str/split l #"")))
              (str/split-lines group))))))
    0
    (str/split input #"\n\n")))
