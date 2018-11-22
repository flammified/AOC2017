(ns day05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (-> "day5/input.txt" io/resource io/file slurp))

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

(defn part-1 []
  (steps-to-termination-part-1 (parse-input input) 0 0))

(defn part-2 []
  (steps-to-termination-part-2 (parse-input input) 0 0))
