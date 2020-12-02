(ns day21
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-async]]
            [clojure.core.async :as async :refer [poll! >!! <!! to-chan chan offer!]]
            [util.grids :as grids :refer [draw-sparse]]))


(def input
  (-> "day21/input.txt"
      io/resource io/file slurp str/trim
      (intcode/parse-program)))


(defn send-function [in cmd]
  (let [cmds (map #(int %) cmd)]
    (reduce
      (fn [_ c]
        (>!! in c))
      {}
      cmds)))

(defn program-1 []
  (let [[in out] (run-async :jump input)]
    (send-function in "NOT A T\n")
    (send-function in "NOT B J\n")
    (send-function in "OR T J\n")
    (send-function in "NOT C T\n")
    (send-function in "OR T J\n")
    (send-function in "AND D J\n")

    (send-function in "WALK\n")
    (loop []
      (let [text (<!! out)]
        ; (print (char text))
        (if (some? text)
          (cond
            (> text 120) text
            :else (do (print (char text)) (recur)))

          nil)))))

(defn program-2 []
  (let [[in out] (run-async :jump input)]
    (send-function in "NOT A T\n")
    (send-function in "NOT B J\n")
    (send-function in "OR T J\n")
    (send-function in "NOT C T\n")
    (send-function in "OR T J\n")
    (send-function in "AND D J\n")

    (send-function in "NOT E T\n")
    (send-function in "NOT T T\n")
    (send-function in "AND I T\n")
    (send-function in "OR H T\n")
    (send-function in "AND T J\n")
    (send-function in "NOT A T\n")
    (send-function in "OR T J\n")

    (send-function in "RUN\n")
    (loop []
        (if (some? text)
          (cond
            (> text 120) text
            :else (do (print (char text)) (recur)))))))

(defn part-1 []
  (program-1))

(defn part-2 []
  (program-2))
