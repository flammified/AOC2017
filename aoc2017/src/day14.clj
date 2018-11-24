(ns day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer (cl-format)]
            [day10 :as day10]))

(def input "ugkiagan")

(defn hashes [amount key]
  (->> (map vector (repeat key) (range 0 amount))
       (map #(str/join "-" %))))

(defn bit-count
  ([v] (bit-count v 0))
  ([v c] (if (zero? v)
           c
           (recur (bit-and v (dec v)) (inc c)))))

(defn knothash-matrix [key]
  (->> key
       (hashes 128)
       (mapv day10/knothash-decimal)))

(defn count-one [m]
  (->> m
       seq
       (filter #(= \1 %))
       count))


(defn part-1 []
  (->> (knothash-matrix "ugkiagan")
       flatten
       (map bit-count)
       (reduce +)))

(defn square-set? [matrix [row col]]
  (bit-test ((matrix row) (quot col 8)) (- 7 (mod col 8))))

(defn set-square [matrix [row col]]
  (assoc-in matrix [row (quot col 8)] (bit-clear ((matrix row) (quot col 8)) (- 7 (mod col 8)))))

(defn fill-island [matrix [row col]]
  (if (or (>= col 128) (< col 0) (>= row 128) (< row 0) (not (square-set? matrix [row col])))
    matrix
    (let [new-matrix (->> (set-square matrix [row col])
                       ((fn [m] (fill-island m [(inc row) col])))
                       ((fn [m] (fill-island m [row (inc col)])))
                       ((fn [m] (fill-island m [(dec row) col])))
                       ((fn [m] (fill-island m [row (dec col)]))))]
      new-matrix)))

(defn count-islands [matrix]
  (let [coordinates (for [y (range 0 128) x (range 0 128)] [x y])]
    (second (reduce
              (fn [[matrix count] [row col]]
                [(fill-island matrix [row col]) (if (square-set? matrix [row col]) (inc count) count)])
              [matrix 0]
              coordinates))))


(defn part-2 []
  (let [matrix (knothash-matrix input)]
    (count-islands matrix)))
