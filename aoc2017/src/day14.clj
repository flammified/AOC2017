(ns day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer (cl-format)]
            [day10 :as day10]))

(def input "ugkiagan")

(defn hashes [amount key]
  (->> (map vector (repeat key) (range 0 amount))
       (map #(str/join "-" %))))

(defn hex-to-bin-hash [hash]
  (->> hash
       seq
       (map #(cl-format nil "~b" %))
       (str/join "")))

(defn knothash-matrix [key]
  (->> key
       (hashes 128)
       (map day10/knothash-decimal)
       (map hex-to-bin-hash)))

(defn count-one [m]
  (->> m
    seq
    (filter #(= \1 %))
    count))


(defn part-1 []
  (->> (knothash-matrix "ugkiagan")
       (map count-one)
       (reduce +)))
