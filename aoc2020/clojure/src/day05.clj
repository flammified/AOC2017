(ns day05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]))


(def input
  (-> "day05/input.txt" io/resource io/file slurp str/trim str/split-lines))


(defn calc-pos [s l f]
  (if (> (count l) 1)

    (if (= (first s) f)
      (recur (drop 1 s) (drop (/ (count l) 2) l) f)
      (recur (drop 1 s) (take (/ (count l) 2) l) f))
    (first l)))

(defn calc-id [s]
  (let [row (calc-pos (drop-last 3 (vec s)) (range 128) \B)
        col (calc-pos (take-last 3 (vec s)) (range 8) \R)]
    (+ (* row 8) col)))

(defn part-1 []
  (apply max (map calc-id input)))

(defn part-2 []
  (first (set/difference (set (range 11 850)) (set (map calc-id input)))))
