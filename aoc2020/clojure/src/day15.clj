(ns day15
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [util.grids :as grids]
            [clojure.math.numeric-tower :as math]))


(def input  [14,8,16,0,1,17])

(defn memory-game [until]
  (loop [mem (reduce-kv (fn [m i v] (assoc m v [(inc i)])) {} input)
         i (inc (count input))
         prev (last input)]
    (if (= i (inc until))
      prev
      (let [val (get mem prev)
            cur-val (case (count val) 1 0 (reduce - (reverse val)))]
        (recur
              (-> mem
                  (update cur-val conj i)
                  (update cur-val vec)
                  (update cur-val (fn [l]
                                    (if (> (count l) 2)
                                      (drop 1 l)
                                      l)))
                  (update cur-val vec))
              (inc i)
              cur-val)))))


(defn part-1 []
  (memory-game 2020))

(defn part-2 []
  (memory-game 30000000))
