(ns day08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn])

  (:use [loom.graph]
        [loom.alg]))

(def input
  (-> "day08/input.txt"
      io/resource io/file slurp str/trim (str/split #"")
      (->> (map edn/read-string) (partition (* 25 6)))))

(defn part-1 []
  (let [layer (apply min-key #(count (filter #{0} %)) input)]
    (* (count (filter #{1} layer)) (count (filter #{2} layer)))))

(defn part-2 []
  (->> input
      (apply map vector)
      (map (fn [line] (drop-while #(= 2 %) line)))
      (map first)
      (partition 25)
      (map #(apply str %))
      (map #(str/replace % #"0" " "))
      (map #(str/replace % #"1" "█"))
      (str/join "\n")))


(println (part-1))
