(ns day5.core
  (require [clojure.string :as str]))

(def input (slurp "input-day5.txt"))
(def test-input)

(defn parse-input [text]
  (vec (map read-string (str/split-lines text))))

(defn out-of-bounds? [stack position]
  (or (>= position (count stack)) (< position 0)))

(defn new-offset [offset]
  (if (>= offset 3)
    (dec offset)
    (inc offset)))

(defn steps-to-termination-part-1 [stack position n]
  (let [done (out-of-bounds? stack position)]
    (if done
      n
      (let [steps (get stack position)]
        (recur (update-in stack [position] inc) (+ position steps) (inc n))))))

(defn steps-to-termination-part-2 [stack position n]
  (let [done (out-of-bounds? stack position)]
    (if done
      n
      (let [steps (get stack position)]
        (recur (update-in stack [position] new-offset) (+ position steps) (inc n))))))


(defn -main []
  (println (steps-to-termination-part-2 (parse-input input) 0 0)))
