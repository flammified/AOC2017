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

(defn choose-dir [pos chosen grid]
  (first
    (filter
        #(and
           (not (contains? (set (get chosen pos)) %))
           (not (= :wall (get grid (grids/step pos %)))))
      [:north :east :south :west])))

(defn explore-maze []
  (let [[in out] (run-async :block input)]
    (loop [stack []
           position [0 0]
           chosen-dirs {}
           grid {}]
      (do
        (let [direction (choose-dir position chosen-dirs grid)]
          (if (nil? direction)
            (let [direction (grids/opposite (last stack))
                  send? (>!! in (grids/dir direction))
                  status (<!! out)]
              (recur (pop stack)
                     (grids/step position direction)
                     chosen-dirs
                     grid))
            (let [send? (>!! in (grids/dir direction))
                  status (<!! out)]
              (case status
                0 (recur
                      stack
                      position
                      (update chosen-dirs position conj direction)
                      (assoc grid (grids/step position direction) :wall))
                1 (recur
                      (conj stack direction)
                      (grids/step position direction)
                      (update chosen-dirs position conj direction)
                      (assoc grid (grids/step position direction) :empty))
                2 (do
                    [(grids/step position direction)
                     (keys (filter (fn [[k v]] (= v :empty)) grid))])))))))))


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


(defn part-1 []
  (let [[tank positions] (explore-maze)]
    [tank positions]))

(defn part-2 []
  (let [[tank positions] (explore-maze)]
    (time-until-filled tank positions)))
