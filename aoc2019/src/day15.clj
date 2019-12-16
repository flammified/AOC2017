(ns day15
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-async]]
            [clojure.core.async :refer [>!! <!! to-chan chan offer!]]
            [util.grids :as grids :refer [draw-sparse]]
            [clojure.set :as set]))

(def input
  (-> "day15/input.txt"
      io/resource io/file slurp str/trim
      (intcode/parse-program)))

(defn choose-dir [pos grid]
  (first
    (filter
      #(not (some? (get grid (grids/step pos %))))
      [:north :east :south :west])))

(defn explore-maze []
  (let [[in out] (run-async :block input)]
    (loop [stack []
           position [0 0]
           grid {}
           distance 0]
        (let [direction (choose-dir position grid)
              previous-direction (last stack)]
          (if (nil? direction)
            (if (nil? previous-direction)
              grid
              (let [send? (>!! in (grids/dir (grids/opposite previous-direction)))
                    status (<!! out)]
                (recur (pop stack)
                       (grids/step position (grids/opposite previous-direction))
                       grid
                       (dec distance))))

            (let [send? (>!! in (grids/dir direction))
                  status (<!! out)]
              (case status
                0 (recur
                      stack
                      position
                      (-> grid
                          (assoc-in [(grids/step position direction) :type] :wall)
                          (assoc-in [(grids/step position direction) :distance] (inc distance)))
                      distance)
                1 (recur
                      (conj stack direction)
                      (grids/step position direction)
                      (-> grid
                          (assoc-in [(grids/step position direction) :type] :empty)
                          (assoc-in [(grids/step position direction) :distance] (inc distance)))
                      (inc distance))
                2 (recur
                    (conj stack direction)
                    (grids/step position direction)
                    (-> grid
                        (assoc-in [(grids/step position direction) :type] :tank)
                        (assoc-in [(grids/step position direction) :distance] (inc distance)))
                    (inc distance)))))))))


(defn propagate-oxygen [oxygen positions]
  (reduce
    (fn [current next]
      (concat current
              (filter (fn [n] (and (contains? (set positions) n)
                                   (not (contains? (set oxygen) n))))
                      (grids/neigh next))))
    oxygen
    oxygen))

(defn time-until-filled [initial-position maze]
  (loop [oxygen [initial-position]
         positions (set maze)
         i 0]
    (if (empty? positions)
      i
      (let [next-oxygen (propagate-oxygen oxygen positions)]
        (recur
          next-oxygen
          (set/difference (set positions) (set next-oxygen))
          (inc i))))))

(defn draw-maze [maze]
  (spit "maze.txt"
    (draw-sparse
      (assoc maze [0 0] {:type :start :distance 0})
      #(case (:type %2) :start "⭕️" :wall "█" :empty " " :tank "✅" " "))))

(defn get-tank [maze]
  (first (filter (fn [[k v]] (= (:type v) :tank)) maze)))

(defn part-1 []
  (let [grid (explore-maze)
        [coord square] (get-tank grid)]
    (draw-maze grid)
    (:distance square)))

(defn part-2 []
  (let [grid (explore-maze)
        [coord square] (get-tank grid)]
    (time-until-filled
      coord
      (keys (filter (fn [[k v]] (= (:type v) :empty)) grid)))))
