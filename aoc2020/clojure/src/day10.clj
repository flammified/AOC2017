(ns day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def input
  (-> "day10/input.txt"
      io/resource
      io/file slurp
      str/trim
      str/split-lines
      (->> (map read-string))
      (vec)
      ((fn [a] (conj a (+ 3 (apply max a)))))
      (conj 0)))


(defn part-1 []
  (loop [cur 0
         list (set input)
         differences []]
    (if (empty? list)
      (* (count (filter #{1} differences)) (count (filter #{3} differences)))
      (recur
        (cond
          (some #{(+ 1 cur)} list) (+ 1 cur)
          (some #{(+ 2 cur)} list) (+ 2 cur)
          (some #{(+ 3 cur)} list) (+ 3 cur)
          :else cur)

        (cond
          (some #{(+ 2 cur)} list) (disj list (+ cur 1))
          (some #{(+ 1 cur)} list) (disj list (+ cur 2))
          (some #{(+ 3 cur)} list) (disj list (+ cur 3))
          :else (set []))
        (cond
          (some #{(+ 1 cur)} list) (conj differences 1)
          (some #{(+ 2 cur)} list) (conj differences 2)
          (some #{(+ 3 cur)} list) (conj differences 3)
          :else differences)))))


(def steps {0 0 1 1 2 2 3 4 4 7})

(defn to-deltas [l]
  (loop [list (drop 1 l)
         cur (first l)
         deltas []]
    (if (empty? list)
      deltas
      (recur (drop 1 list) (first list) (conj deltas (- (first list) cur))))))

(defn part-2 []
  (let [sorted (sort input)
        deltas (to-deltas sorted)
        split (partition-by #(= 1 %) deltas)
        ones (take-nth 2 split)]
    (reduce * (map steps (map count ones)))))



(part-2)
