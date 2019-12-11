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
  (Math/atan2 (- fx tx) (- fy ty)))

(defn distance [[fx fy] [tx ty]]
  (math/sqrt (+ (math/expt (- tx fx) 2) (math/expt (- ty fy) 2))))

(defn normalize [a]
  (* -1 (+ a (/ Math/PI 2))))

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
  (max-key count
         (for [p asteroids]
           (-> asteroids
               (to-angles p)))))

(defn part-1 []
  (best-square input))

(defn part-2 []
  (let [grid (to-angles input (best-square input))
        angles (sort (keys grid))]
    ; (println angles)
    (loop [grid grid
           vaporized []]
      ; (println (count vaporized))
      (if (>= (count vaporized) 200)
        vaporized
        (recur (reduce #(update %1 %2 pop) grid angles) (concat vaporized (filter some? (->> angles (map (partial get grid)) (map peek)))))))))



(println (part-1))
