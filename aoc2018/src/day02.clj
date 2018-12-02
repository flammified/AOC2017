(ns day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

(def input
  (-> "day02/input.txt"
      io/resource io/file slurp
      (->>
        (str/split-lines))))

(defn exact-amount [lines amount]

  (->> lines
       (map frequencies)
       (map vals)
       (filter (fn [freqs] (some #(#{amount} %) freqs)))
       count))


(defn remove-at [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn remove-every [line]
  (map #(str (apply str (remove-at (-> line seq vec) %)) "-" %) (range (count line))))

(defn duplicate [coll]
  (reduce
    (fn [seen item]
      (if (seen item)
        (reduced item)
        (assoc seen item :seen)))
    {}
    coll))

(defn one-off [lines]
  (->> lines
       (mapcat remove-every)
       duplicate))

(defn part-1 []
  (* (exact-amount input 2) (exact-amount input 3)))

(defn part-2 []
  (one-off input))
