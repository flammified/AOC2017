(ns day17
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-async]]
            [clojure.core.async :as async :refer [poll! >!! <!! to-chan chan offer!]]
            [util.grids :as grids :refer [draw-sparse]]))


(def input
  (-> "day17/input.txt"
      io/resource io/file slurp str/trim
      (intcode/parse-program)))


(defn create-grid [input]
  (let [[in out] (run-async :image input)]
    (loop [grid {}
           position [0 0]]
      (let [tile (<!! out)]
        (case tile
          nil [in out grid]
          10 (recur grid (assoc (grids/step position :south) 0 0));)))))))
          46 (recur (assoc grid position "∼") (grids/step position :east));)))))))
          35 (recur (assoc grid position "#") (grids/step position :east));)))))))
          58 (do (println (<!! out)) [in out grid])
          (recur (assoc grid position (char tile)) (grids/step position :east)))))));))))))))))

(defn intersections [grid]
  (keep (fn [coord]
            (let [up (grids/step coord :north)
                  down (grids/step coord :east)
                  left (grids/step coord :south)
                  right (grids/step coord :west)]
              (if (and (= "#" (get grid up))
                       (= "#" (get grid down))
                       (= "#" (get grid left))
                       (= "#" (get grid right))
                       (= "#" (get grid coord)))
                  (* (first coord) (second coord))
                  nil)))
        (keys grid)))

(defn send-function [in cmd]
  (let [cmds (map #(int %) cmd)]
    (reduce
      (fn [_ c]
        (>!! in c))
      {}
      cmds)))

(defn functions [in out]
  (do
    (send-function in "A,B,A,C,A,B,C,B,C,A\n")
    (send-function in "L,12,R,4,R,4,L,6\n")
    (send-function in "L,12,R,4,R,4,R,12\n")
    (send-function in "L,10,L,6,R,4\n")
    (send-function in "n\n")
    (loop []
      (let [text (<!! out)]
        (println text)
        (if (some? text)
          (cond
            (> text 120) text
            :else (do (print (char text)) (recur)))

          nil)))))

(defn part-1 []
  (let [[in out grid] (create-grid input)]
    (reduce + (filter some? (intersections grid)))))

(defn part-2 []
  (let [[in out grid] (create-grid (assoc input 0 2))]
    (functions in out)))

(part-2)
