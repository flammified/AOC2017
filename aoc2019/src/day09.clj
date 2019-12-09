(ns day07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.async :refer [>!! <!! chan]]
            [util.intcode :refer [run-sync]]))

(def input
  (-> "day09/input.txt"
      io/resource io/file slurp str/trim
      (str/split #",")
      (->> (map edn/read-string) (into []))))

(defn part-1 []
  (let [inp (chan 1)
        outp (chan 1)]
    (>!! inp 1)
    (run-sync input inp outp)
    (<!! outp)))

(defn part-2 []
  (let [inp (chan 1)
        outp (chan 1)]
    (>!! inp 2)
    (run-sync input inp outp)
    (<!! outp)))
