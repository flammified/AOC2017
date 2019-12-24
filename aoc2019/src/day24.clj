(ns day24
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.grids :as grids]
            [clojure.math.numeric-tower :as math]))

(def input
  (-> "day24/input.txt"
      io/resource io/file slurp
      (str/split-lines)
      (->> (mapv #(vec (char-array %))))))

(defn gol [grid]
  (reduce
    (fn [new [x y layer]]
      (let [tile (get grid [x y layer])
            amount (reduce
                     (fn [n direction]
                       (if (= (get grid (conj (grids/step [x y] direction) 0))
                              \#)
                         (inc n)
                         n))
                     0
                     [:north :east :south :west])]
        (assoc new [x y layer]
          (cond
            (and (= tile \.) (or (= amount 1) (= amount 2)))
            \#
            (and (= tile \#) (= amount 1))
            \#
            (and (= tile \#) (not (= amount 1)))
            \.
            :else \.))))
    grid
    (keys grid)))

(defn find-in-map [hm val]
  (first
    (filter
      #(= val (get hm %))
      (keys hm))))

(defn first-duplicate [coll]
  (loop [s #{}
         coll coll]
    (if (contains? s (first coll))
        (first coll)
        (recur (conj s (first coll)) (drop 1 coll)))))

(defn char-array-to-map [ca]
  (into {}
    (for [y (range (count ca))
          x (range (count (get ca 0)))]
      [[x y 0] (get-in ca [y x])])))

(defn biodiv [grid]
  (reduce
    (fn [i [x y layer]]
      ; (println (get grid [x y layer]))
      (if (not (= \# (get grid [x y layer])))
        i
        (+ i (math/expt 2 (+ (* y 5) x)))))
    0
    (keys grid)))


(defn part-1 []
  (let [grid (char-array-to-map input)]
     (biodiv (first-duplicate (iterate gol grid)))))

(defn down [dir]
  (case dir
    :north [[0 0] [1 0] [2 0] [3 0] [4 0]]
    :east [[4 0] [4 1] [4 2] [4 3] [4 4]]
    :south [[0 4] [1 4] [2 4] [3 4] [4 4]]
    :west [[0 0] [0 1] [0 2] [0 3] [0 4]]))

(defn up [dir]
  (case dir
    :north [2 1]
    :east [3 2]
    :south [2 3]
    :west [1 2]))

(defn neighbours [grid [x y layer]]
  (reduce
    (fn [res direction]
      (conj res
        (let [[nx ny] (grids/step [x y] direction)]
          (if (= (grids/step [x y] direction) [2 2])
            (map #(conj % (dec layer)) (down direction))
            (cond
              (and (= direction :north) (= ny -1))
              [(conj (up :south) (inc layer))]
              (and (= direction :east) (= nx 5))
              [(conj (up :west) (inc layer))]
              (and (= direction :west) (= nx -1))
              [(conj (up :east) (inc layer))]
              (and (= direction :south) (= ny 5))
              [(conj (up :north) (inc layer))]
              :else [(conj (grids/step [x y] direction) layer)])))))
    []
    [:north :east :south :west]))

(defn propagate-bugs [grid]
  (let [counts (->> (keys grid)
                    (map (partial neighbours grid))
                    (apply concat)
                    (apply concat)
                    (frequencies))]
    (into {} (filter second
               (reduce
                 (fn [new coord]
                   (let [tile (or (get grid coord) \.)
                         amount (or (get counts coord) 0)]
                     ; (println tile coord amount)
                     (assoc new coord
                       (cond
                         (and (= tile \.) (or (= amount 1) (= amount 2)))
                         \#
                         (and (= tile \#) (= amount 1))
                         \#
                         (and (= tile \#) (not (= amount 1)))
                         nil
                         :else nil))))
                 grid
                 (vec (distinct (concat (keys grid) (keys counts)))))))))

(defn part-2 []
  (let [grid (->> input
                  (char-array-to-map)
                  (filter (fn [[k v]] (not (= \. v))))
                  (into {}))]
    (-> (iterate propagate-bugs grid)
        (nth 200)
        (->> (filter (fn [[k v]] (= v \#)))
             (count)))))


(println (part-2))
