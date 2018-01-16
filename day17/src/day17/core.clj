(ns day17.core)

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

(defn index-1-after-x  [x steps])





(defn -main []
  (let [part1 (spinlock 2017 394)
        part2 (index-1-after-x 50000000 394)]
    (println (around part1 2017 5))
    (println part2)))
