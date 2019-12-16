(ns day16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

  ; (:use [loom.graph]
        ; [loom.alg]))

(def input
  (-> "day16/input.txt"
      io/resource io/file slurp str/trim (str/split #"")
      (->> (mapv #(edn/read-string %)))))

(defn last-digit [s]
  (edn/read-string (str (last (str s)))))

(defn stretch [signal length]
  (->> signal
       (map #(take length (repeat %)))
       (apply concat)))

(defn phase [signal]
  (reduce
    (fn [res idx]
      (let [base [0 1 0 -1]
            stretched (stretch base (inc idx))]
        (assoc res idx (last-digit (math/abs (reduce + (map * (drop 1 (cycle stretched)) signal)))))))
    signal
    (range (count signal))))


(defn part-1 []
  (->> (reduce
         (fn [signal _]
           (phase (vec signal)))
         input
         (range 100))
       (take 8)
       (apply str)))


(def input-2 (->> input
                  (#(identity [%]))
                  (cycle)
                  (take 10000)
                  (apply concat)
                  (into [])))

(defn phase-2 [signal]
  (reduce
    #(let [prev (first %1)
           next (mod (+ (get signal %2) prev) 10)]
      (conj %1 next))
    (list (last signal))
    (range (- (count signal) 2) -1 -1)))

(defn part-2 []
  (let [offset (edn/read-string (apply str (take 7 input)))]
    (->> (reduce
           (fn [signal _]
             (phase-2 (vec signal)))
           (drop offset input)
           (range 100))
         (take 8)
         (apply str))))
