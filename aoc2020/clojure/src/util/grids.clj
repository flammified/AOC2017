(ns util.grids
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))


(def cardinals [:north :east :south :west])
(def extended-cardinals [:north :northeast :east :southeast :south :southwest :west :northwest])

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (math/abs (- x2 x1)) (math/abs (- y2 y1))))

(defn aabb [nodes]
  (let [max-x (apply max (map first nodes))
        max-y (apply max (map second nodes))]
    [max-x max-y]))

(defn aabb-min [nodes]
  (let [min-x (apply min (map first nodes))
        min-y (apply min (map second nodes))]
    [min-x min-y]))

(defn reduce-grid [grid f]
  (reduce-kv
    (fn [grid y line]
      (reduce-kv
        (fn [grid x ch]
          (f grid [x y] ch))
        grid
        (vec line)))
    {}
    (vec grid)))

(defn dir [direction]
  (case direction
    :north 1
    :south 2
    :west 3
    :east 4))

(defn right-side [direction]
  (case direction
    :east :south
    :south :west
    :west :north
    :north :east))

(defn left-side [direction]
  (case direction
    :east :north
    :south :east
    :west :south
    :north :west))


(defn opposite [direction]
  (case direction
    :east :west
    :south :north
    :west :east
    :north :south))

(defn rotate [way dir]
  (case way
    :left (left-side dir)
    :right (right-side dir)
    0 (left-side dir)
    1 (right-side dir)))

(defn direction-to-vector [direction]
  (case direction
    :east [1 0]
    :west [-1 0]
    :north [0 -1]
    :south [0 1]
    :northeast [1 -1]
    :southeast [1 1]
    :southwest [-1 1]
    :northwest [-1 -1]))

(defn step [pos dir]
  (mapv + pos (direction-to-vector dir)))

(defn neigh [pos]
  (map (partial step pos) cardinals))

(defn neigh-diagonal [pos]
  (map (partial step pos) extended-cardinals))

(defn draw-sparse
  ([grid ch]
   (if (not (empty? (keys grid)))
      (let [nodes (keys grid)
            [maxx maxy] (aabb nodes)
            [minx miny] (aabb-min nodes)]
        (draw-sparse grid ch minx miny maxx maxy))))
  ([grid ch minx miny maxx maxy]
   (let [draw-str (str/join "\n"
                    (doall
                      (for [y (range miny (inc maxy))]
                        (str/join
                          (for [x (range minx (inc maxx))]
                            (do
                              (ch [x y] (get grid [x y]))))))))]

     (println draw-str)
     draw-str)))
