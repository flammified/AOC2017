(ns day17)

(defn around [values value x]
  (let [index (.indexOf values value)
        rear (- index x)
        front (+ index x)]
    (subvec values rear front)))

(defn spinlock
  ([x steps] (spinlock x steps 0 [0] 1))
  ([x steps cursor state n]
   (let [next-cursor (mod (+ cursor steps) (count state))
         [before after] (split-at (inc next-cursor) state)
          new-state (vec (concat before [n] after))]
     (if (== n (inc x))
       state
       (recur x steps (inc next-cursor) new-state (inc n))))))

(defn index-1-after-x  [x steps]
  (reduce
    (fn [[cursor value] next]
      (let [next-cursor (+ 1 (mod (+ cursor steps) next))]
        (if (== next-cursor 1)
          [next-cursor next]
          [next-cursor value])))
    [0 0]
    (range 1 x)))

(defn part-1 []
  (around (spinlock 2017 394) 2017 5))

(defn part-2 []
  (index-1-after-x 50000000 394))
