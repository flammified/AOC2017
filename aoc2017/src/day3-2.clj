(ns day3-2.core)

(def directions
  [[-1 -1]
   [-1 0]
   [-1 1]
   [0 -1]
   [0 1]
   [1 1]
   [1 0]
   [1 -1]])

(defn right-side [direction]
  (case direction
    :east :south
    :south :west
    :west :north
    :north :east))

(defn direction-to-vector [direction]
  (case direction
    :east [1 0]
    :west [-1 0]
    :north [0 -1]
    :south [0 1]))

(defn set-in-spiral [spiral [x y] value]
  (assoc spiral (str x "-" y) value))

(defn current-value-at-coordinate [spiral [x y]]
  (get spiral (str x "-" y)))

(defn apply-vector [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn value-at-offset [spiral location offset]
  (current-value-at-coordinate spiral (apply-vector location offset)))

(defn sum-surrounding-values [spiral coordinate]
  (reduce +
    (map
        #(or (value-at-offset spiral coordinate %) 0)
        directions)))

(defn new-value-at-coordinate [spiral n coordinate]
  (case n
    0 1
    (sum-surrounding-values spiral coordinate)))

(defn free? [spiral location]
  (nil? (current-value-at-coordinate spiral location)))

(defn right-free? [spiral location direction]
  (free? spiral (apply-vector location (direction-to-vector (right-side direction)))))

(defn new-direction [spiral n location direction]
  (case n
    0 :east
    1 :south
    (if (right-free? spiral location direction)
      (right-side direction)
      direction)))

(defn spiral [spiral n location direction limit]
  (let [new-value (new-value-at-coordinate spiral n location)]
    (if (> new-value limit)
      new-value
      (let [next-direction (new-direction spiral n location direction)
            next-location (apply-vector location (direction-to-vector next-direction))
            new-spiral (set-in-spiral spiral location new-value)]
        (recur new-spiral (+ n 1) next-location next-direction limit)))))

(defn highest-number-in-spiral-below-limit [limit]
  (spiral {} 0 [0 0] :east limit))

(defn -main []
  (println (highest-number-in-spiral-below-limit 312051)))
