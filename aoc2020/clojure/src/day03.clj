(ns day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))


(defn parse [input]
  {:grid
   (reduce-kv
     (fn [grid y line]
       (reduce-kv
         (fn [grid x c]
           (assoc grid [x y] c))
         grid
         (vec line)))
     {}
     (str/split-lines input))
   :width (count (get (str/split-lines input) 0))
   :height (count (str/split-lines input))})

(def input
  (-> "day03/input.txt" io/resource io/file slurp str/trim parse))


(defn walk [grid h w [dx dy]]
  (loop [x 0
         y 0
         trees 0]

    (if (> y h)
      trees
      (recur (+ x dx) (+ y dy) (if (= \# (get grid [(mod x w) y])) (inc trees) trees)))))


(defn part-1 []
  (walk (:grid input) (:height input) (:width input) [3 1]))


(defn part-2 []
  (apply * (map #(walk (:grid input) (:height input) (:width input) %) [[1 1] [3 1] [5 1] [7 1] [1 2]])))
