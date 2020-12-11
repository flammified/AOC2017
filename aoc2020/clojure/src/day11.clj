(ns day11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [util.grids :as grids]
            [utils.collections :as coll]))

(defn parse [lines]
  (let [height (count lines)
        width (count (get lines 0))]
    {:height height
     :width width
     :grid (grids/reduce-grid lines
             (fn [grid pos ch]
               (assoc grid pos (case ch \L :free \# :occupied \. :floor :floor))))}))


(defn trace [grid position dir]
  (loop [pos (grids/step position dir)]
    ; (println "CONSIDER " pos (get grid pos))
    (if (not (contains? grid pos))
      nil
      (if (not (= (get grid pos) :floor))
        (get grid pos)
        (recur (grids/step pos dir))))))



(defn gol [g width height]
  (reduce
    (fn [grid coord]
      (let [neighbours (grids/neigh-diagonal coord)
            status (get g coord)]
        (case status
          :free
              (assoc grid coord
                (case (count (filter #(= :occupied %) (map #(get g %) neighbours)))
                  0 :occupied
                  :free))
          :occupied
              (assoc grid coord
                (case (count (filter #(= :occupied %) (map #(get g %) neighbours)))
                  4 :free
                  5 :free
                  6 :free
                  7 :free
                  8 :free
                  :occupied))
          :floor (assoc grid coord :floor))))
    g
    (for [x (range width)
          y (range height)]
      [x y])))


(defn gol2 [g width height]
  (reduce
    (fn [grid coord]
      (let [neighbours (map #(trace g coord %) grids/extended-cardinals)
            status (get g coord)]
        (case status
          :free
              (assoc grid coord
                (case (count (filter #(#{:occupied} neighbours)))
                  0 :occupied
                  :free))
          :occupied
              (assoc grid coord
                (case (count (filter #(#{:occupied} neighbours)))
                  5 :free
                  6 :free
                  7 :free
                  8 :free
                  :occupied))
          :floor (assoc grid coord :floor))))
    g
    (for [x (range width)
          y (range height)]
      [x y])))

(def input
  (-> "day11/input.txt"
      io/resource
      io/file slurp
      str/trim
      str/split-lines
      parse))


(defn count-occupies [grid]
  (count (filter (fn [[p s]] (= s :occupied)) grid)))

(defn part-1 []
  (let [{:keys [width height grid]} input]
    (-> (iterate #(gol % width height) grid)
        (coll/first-duplicate)
        (count-occupies))))

(defn part-2 []
  (let [{:keys [width height grid]} input]
    (-> (iterate #(gol2 % width height) grid)
        (coll/first-duplicate)
        (count-occupies))))


(part-1)
