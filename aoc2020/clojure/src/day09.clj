(ns day09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(defn parse [])

(def input
  (-> "day09/input.txt"
      io/resource
      io/file slurp
      str/trim
      str/split-lines
      (->> (map read-string))
      (vec)))

(defn contains-sum? [l v]
  (not (empty?
         (filter
           #{v}
           (for [x l
                 y l
                 :when (not (= x y))]
             (+ x y))))))


(defn find-first-invalid-number [l]
  (->> (partition 26 1 input)
       (keep-indexed (fn [i val] (if
                                   (not (contains-sum? (drop-last val) (last val)))
                                   [i (last val)]
                                   nil)))

       (first)))


(defn part-1 []
  (second (find-first-invalid-number input)))
(defn part-2 []
  (let [[index val] (find-first-invalid-number input)
        sublist (first
                  (filter
                    #(= (reduce + %) val)
                    (for [x (range index)
                          y (range index)
                          :when (< x y)]
                      (->> input
                           (drop x)
                           (take (- y x))))))]
    (+ (apply min sublist) (apply max sublist))))


(part-2)
