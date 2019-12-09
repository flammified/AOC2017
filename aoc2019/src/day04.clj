(ns day04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(def start 123257)
(def end 647015)

(defn all-ascending? [s]
  (->> s
      (str)
      (vec)
      (map str)
      (map #(Integer/parseInt %))
      (apply <=)))


(defn contains-groups-of-two? [n]
  (<= 1 (count (filter #(= 2 %) (vals (frequencies (str n)))))))

(defn contains-groups-of-at-least-two? [n]
  (<= 1 (count (filter #(<= 2 %) (vals (frequencies (str n)))))))

(defn correct-1? [s]
  (and (all-ascending? s)
       (contains-groups-of-at-least-two? s)))

(defn correct-2? [s]
  (and (all-ascending? s)
       (contains-groups-of-two? s)))

(defn part-1 []
  (count (filter correct-1? (range start end))))
(defn part-2 []
  (count (filter correct-2? (range start end))))
