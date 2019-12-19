(ns day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-sync]]
            [clojure.core.async :as async :refer [poll! >!! <!! to-chan chan offer!]]
            [util.grids :as grids :refer [draw-sparse]]))


(def input
  (-> "day19/input.txt"
      io/resource io/file slurp str/trim
      (intcode/parse-program)))


(defn count-1s [input]
    (loop [c 0
           x 0
           y 0]
      (let [out (chan 1)
            _ (run-sync input (async/to-chan [x y]) out)]
        (if (= y 50)
          c
          (let [status (<!! out)]
            (recur (if (pos? status) (inc c) c) (if (< x 50) (inc x) 0) (if (= x 49) (inc y) y)))))))

(defn get-pos [[x y]]
  (let [[in out] (run-async :beam input)]
    (do
      (>!! in x)
      (>!! in y)
      (let [status (<!! out)]
        status))))

(defn below [[x y]]
  (count (take-while #(do (= 1 (get-pos %))) (iterate #(grids/step % :south) [x y]))))

(defn right [[x y]]
  (count (take-while #(= 1 (get-pos %)) (iterate #(grids/step % :east) [x y]))))

(def right (memoize right))

(defn part-1 []
  (count-1s input))
;
(defn part-2 []
  (filter
    (fn [[x y]]
      (let [b (below [x y])
            r (right [x y])]
        (println [x y] b r)
        (and (>= b 100)
             (>= r 100))))

    (for [y (range 1248 1400) x (range 800 1400)] [x y])))

(println (part-1))
