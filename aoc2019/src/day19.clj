(ns day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-sync]]
            [clojure.core.async :as async :refer [poll! >!! <!! to-chan chan offer!]]
            [util.grids :as grids :refer [draw-sparse]]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))


(tufte/add-basic-println-handler! {})


(def input
  (-> "day19/input.txt"
      io/resource io/file slurp str/trim
      (intcode/parse-program)))

(defn get-pos [[x y]]
  (first (:output (run-sync input [x y]))))

(defn count-1s [input]
  (loop [c 0
         x 0
         y 0]
    (let [status (get-pos [x y])]
      (if (= y 50)
        c
        (recur (if (pos? status) (inc c) c) (if (< x 50) (inc x) 0) (if (= x 49) (inc y) y))))))

(defn below [[x y]]
  (count (take-while #(do (= 1 (get-pos %))) (iterate #(grids/step % :south) [x y]))))

(defn right [[x y]]
  (count (take-while #(= 1 (get-pos %)) (iterate #(grids/step % :east) [x y]))))

(defn part-1 []
  (count-1s input))

(defn part-2 []
  (first
    (filter
      (fn [[x y]]
        (let [b (below [x y])
              r (right [x y])]
          (and (>= b 100)
               (>= r 100))))

      (for [y (range 1247 1249) x (range 1121 1123)] [x y]))))
