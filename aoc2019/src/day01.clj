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


(defn calculate-fuel [a]
  (-> a (/ 3) (int) (- 2)))

(defn fuel-2 [a]
  (->> a
       (iterate calculate-fuel)
       (drop 1)
       (take-while pos?)
       (reduce +)))

(defn part-1 []
  (reduce + (map calculate-fuel input)))

(defn part-2 []
  (reduce + (map fuel-2 input)))
