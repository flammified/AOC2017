(ns day6.core
  (:use [clojure.string :as str]))

(def input (slurp "input.txt"))
(def solution-from-part-1 [0 14 13 12 10 9 9 7 7 5 5 4 2 2 1 12])

(defn parse-input [input]
  (vec (map read-string (str/split input #"\t"))))

(defn largest-key [stack]
  (let []
    (first (keep-indexed #(when (= %2 (apply max stack)) %1) stack))))

(defn duplicates-in-vec [stack]
  (not (== (count (distinct stack)) (count stack))))

(defn next-index [index length]
  (let [next-index (inc index)]
    (if (== length next-index)
      0
      next-index)))

(defn inc-until-empty [stack index n]
  (if (== 0 n)
    stack
    (let [next-stack (update-in stack [index] inc)]
      (recur next-stack (next-index index (count stack)) (dec n)))))

(defn redistribute [stack]
  (let [max-index (largest-key stack)
        amount-to-redistribute (get stack max-index)
        emptied-stack (assoc stack max-index 0)]
    (inc-until-empty emptied-stack (next-index max-index (count stack)) amount-to-redistribute)))

(defn cycles-until [current until n]
  (let [redistributed (redistribute current)]
    (if (== (count (distinct (vector redistributed until))) 1)
      (inc n)
      (recur redistributed until (inc n)))))

(defn first-repeating-configuration [history stack n]
  (if (duplicates-in-vec history)
    stack
    (let [next-stack (redistribute stack)]
      (recur (conj history stack) next-stack (inc n)))))

(defn -main []
  (println (cycles-until solution-from-part-1 solution-from-part-1 0)))
