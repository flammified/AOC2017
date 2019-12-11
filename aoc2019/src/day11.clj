(ns day08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-async]]
            [clojure.core.async :refer [>!! <!! to-chan chan]]
            [util.grids :as grids :refer [draw-sparse]]))

(def input
  (-> "day11/input.txt"
      io/resource io/file slurp str/trim
      (intcode/parse-program)))

(defn panels
  ([input] (panels input {}))
  ([input initial-grid]
   (let [[in out] (run-async :painter input)]
     (loop [position [0 0]
            direction :north
            grid initial-grid]
       (let [cur (get grid position)]
         (do
           (>!! in (if (some? cur) cur 0))
           (let [color (<!! out)
                 dirid (<!! out)
                 new-grid (assoc grid position color)]
             (if (or (nil? color) (nil? direction))
               grid
               (recur
                 (grids/step position (grids/rotate dirid direction))
                 (grids/rotate dirid direction)
                 new-grid)))))))))

(defn part-1 []
  (count (keys (panels input))))

(defn part-2 []
  (let [inp (panels input {[0 0] 1})
        grid (keys inp)]
    (draw-sparse inp)
    nil))
