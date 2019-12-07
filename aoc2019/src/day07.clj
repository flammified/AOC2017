(ns day07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.async :refer [>!! <!!]]
            [util.intcode :refer [run-async]]))

(def input
  (-> "day07/input.txt"
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
  (apply max (filter some? (for [a (range 0 5)
                                 b (range 0 5)
                                 c (range 0 5)
                                 d (range 0 5)
                                 e (range 0 5)]
                             (let [t (str a b c d e)]
                               (if (= (count (set t)) (count t))
                                 (test-amplifier-setup input a b c d e)))))))

(defn part-2 []
  (apply max (filter some? (for [a (range 5 10)
                                 b (range 5 10)
                                 c (range 5 10)
                                 d (range 5 10)
                                 e (range 5 10)]
                             (let [t (str a b c d e)]
                               (if (= (count (set t)) (count t))
                                 (test-amplifier-setup input a b c d e)))))))
