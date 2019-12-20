(ns day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))

(defn rotate [a n]
  (let [l (count a)
        off (mod (+ (mod n l) l) l)]
    (flatten (list (drop off a) (take off a)))))

(defn grid-to-coordinates [grid]
  (->> grid
       (map #(str/split % #""))
       (map-indexed (fn [y line] (keep-indexed (fn [x char]
                                                 (if (= char "#")
                                                   [x y]
                                                   nil))
                                               line)))
       (filter some?)
       (reduce concat)))


(def input
  (-> "day10/input.txt"
      io/resource io/file slurp str/trim
      (str/split-lines)
      (#(identity {:w (count %) :h (count (get % 0)) :asteroids (grid-to-coordinates %)}))))

(defn angle-to [[fx fy] [tx ty]]
  (Math/atan2 (- ty fy) (- tx fx)))

(defn distance [[fx fy] [tx ty]]
  (math/sqrt (+ (math/expt (- fx tx) 2) (math/expt (- fy ty) 2))))

(defn normalize [a]
  (mod (+ (/ Math/PI 2) (if (neg? a) (+ a (* 2 Math/PI)) a)) (* 2 Math/PI)))

(defn to-angles [asteroids [fx fy :as from]]
  (->> asteroids
       (reduce (fn [angles [x y :as coord]]
                 (if (not (= [x y] [fx fy]))
                   (let [normalized-angle (normalize (angle-to [fx fy] [x y]))]
                     (-> (update angles
                                 normalized-angle
                                 #(conj % [x y]))
                         (update normalized-angle
                                 (partial sort-by (partial distance [fx fy])))))
                   angles))
               {})))

(defn best-square [{:keys [w h asteroids]}]
  (apply
    max-key
    :count
    (for [p asteroids]
      {:p p :count (-> asteroids
                       (to-angles p)
                       (keys)
                       (count))})))

(defn find-in-map [hm val]
  (reduce
    (fn [_ k]
      (if (contains? (set (get hm k)) val)
        (reduced k)
        nil))
    (keys hm)))

(defn part-1 []
  (best-square input))

(defn take-while-rotating [asteroids p n]
  (let [grid (to-angles asteroids p)
        angles (sort (keys grid))]
    (loop [grid grid
           vaporized []]
      (if (>= (count vaporized) n)
        vaporized
        (recur
          (reduce #(update
                     %1
                     %2
                     (fn [l] (drop 1 l)))
                   grid
                  angles)
          (concat vaporized
                  (filter
                    some?
                    (->> angles
                         (map (partial get grid))
                         (map first)))))))))


(defn part-2 []
  (nth (take-while-rotating (:asteroids input) (:p (best-square input)) 200) 199))
