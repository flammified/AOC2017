(ns day08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-async]]
            [clojure.core.async :refer [>!! <!! to-chan chan offer!]]
            [util.grids :as grids :refer [draw-sparse]]))

(def input
  (-> "day13/input.txt"
      io/resource io/file slurp str/trim
      (intcode/parse-program)
      (assoc 0 2)))

(defn initial-state
  ([input]
   (let [[in out] (run-async :painter input)]
     (loop [output {}]
       (do
         (let [x (<!! out)
               y (<!! out)
               type (<!! out)]
           (if (not (and (some? x) (some? y)))
             output
             (recur (assoc output [x y] type)))))))))

(defn play [input]
  (let [[in out] (run-async :block input)]
    (loop [output {}
           [lx ly :as last-ball-position] [19 19]
           [lpx lpy :as last-paddle position] [20 2]
           score 0]
      (do
        (let [x (<!! out)
              y (<!! out)
              z (<!! out)]
          (do
            (if (not (and (some? x) (some? y)))
              score
              (case x
                -1 (recur output [lx ly] [lpx lpy] z)
                (case z
                  3 (recur output [lx ly] [x y] score)
                  4 (do
                      (cond
                        (< lpx x) (do (>!! in 1))
                        (> lpx x) (do (>!! in -1))
                        :else (>!! in 0))
                      (recur output [x y] [lpx lpy] score))
                  (recur (assoc output [x y] z) [lx ly] [lpx lpy] score))))))))))

(defn part-1 []
  (initial-state (filter #{2} (vals (initial-state input)))))

(defn part-2 []
  (let [score (play input)]
    (println score)))

(part-2)
