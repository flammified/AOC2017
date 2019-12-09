(ns day09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.async :refer [>!! <!! to-chan chan]]
            [util.intcode :refer [run-sync]]))

(def input
  (-> "day09/input.txt"
      io/resource io/file slurp str/trim
      (str/split #",")
      (->> (map edn/read-string) (into []))))

(defn part-1 []
  (let [inp (to-chan [1])
        outp (chan 2)]
    (>!! inp 1)
    (run-sync input inp outp)
    (<!! (clojure.core.async/into [] outp))))

(defn part-2 []
  (let [inp (to-chan [2])
        outp (chan 2)]
    (run-sync input inp outp)
    (<!! (clojure.core.async/into [] outp))))

(println (part-1))
(println (part-2))
