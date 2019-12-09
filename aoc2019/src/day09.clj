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


(defn test-amplifier-setup [program a b c d e]
  (let [amplifiers {:a (run-async :a program)
                    :b (run-async :b program)
                    :c (run-async :c program)
                    :d (run-async :d program)
                    :e (run-async :e program)}]

    (>!! (-> amplifiers :a first) a)
    (>!! (-> amplifiers :b first) b)
    (>!! (-> amplifiers :c first) c)
    (>!! (-> amplifiers :d first) d)
    (>!! (-> amplifiers :e first) e)

    (try
      (loop [previous 0
             next (cycle [:a :b :c :d :e])]

        (let [[in out] (get amplifiers (first next))]
          (do
            (>!! in previous)
            (let [output (<!! out)]
              (if (nil? output)
                previous
                (recur output (drop 1 next))))))))))

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
