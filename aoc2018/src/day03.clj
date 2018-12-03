(ns day03
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

;#1256 @ 277,6: 29x20

(defn parse-line [line]
  (let [[id _ point dimensions] (str/split line #" ")
        [x-string y-string] (str/split point #",")
        [x y] (map read-string [x-string (str/join "" (drop-last y-string))])
        [w h] (map read-string (str/split dimensions #"x"))]

    [id [x y] (mapv + [x y] [w h])]))


(def text
  (-> "day03/input.txt" io/resource io/file slurp))

(def input (->> text (str/split-lines) (map parse-line)))

(defn points [[id [x1 y1] [x2 y2]]]
  (for [x (range x1 x2)
        y (range y1 y2)]
    [x y]))

(defn rects->grid [rectangles]
  (reduce
    (fn [grid [id coordinate :as rectangle]]
      (reduce (fn [g point] (update-in g [point] conj id)) grid (points rectangle)))
    {}
    rectangles))

(defn intersections [rectangles]
  (let [grid (rects->grid rectangles)]
    (->> grid
         vals
         (filter #(> (count %) 1))
         count)))

(defn no-overlap [rectangles]
  (let [grid (rects->grid rectangles)
        ids (->> rectangles (map first) set)
        ids-with-intersections (->> grid
                                    vals
                                    (filter #(> (count %) 1))
                                    flatten
                                    set)]
    (first (filter #(not (contains? ids-with-intersections %)) ids))))



(defn part-1 []
  (intersections input))

(defn part-2 []
  (no-overlap input))
