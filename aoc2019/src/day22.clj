(ns day22
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.modular :refer [mmi]]))


(defn parse [line]
  (let [val (first (map edn/read-string (re-seq #"-?\d+" line)))
        type (cond
              (str/includes? line "stack") :stack
              (str/includes? line "cut") :cut
              (str/includes? line "increment") :increment)]
    {:type type :val val}))



(def input
  (-> "day22/input.txt"
      io/resource io/file slurp str/trim
      (str/split-lines)
      (->> (map parse))))

(defn applyf [[a b] x length]
  (mod (+ (* a x) b) length))

(defn compose [[a b] [c d] length]
;     # f(x) = ax + b
;     # g(x) = c * f(x) + d
;     #      = c * (ax+b) + d
;     #      = ca * x + cb + d
  [(mod (* c a) length) (mod (+ (bigint (* c b)) d) length)])

(defn stack-backward [[a b] length]
  (compose [a b] [-1 (dec length)] length))

(defn cut-backward [[a b] length n]
  (compose [a b] [1 n] length))

(defn increment-backward [[a b] length n]
  (compose [a b] [(mmi n length) 0] length))

(defn increment [[a b] length n]
  (compose [a b] [n 0] length))

(defn cut [[a b] length n]
  (compose [a b] [1 (* -1 n)] length))

(defn stack [[a b] length]
  (compose [a b] [-1 (dec length)] length))

(defn actions->function [input function length functions]
  (loop [actions input
         function function]
    (if (empty? actions)
      function
      (recur
        (drop 1 actions)
        (case (:type (first actions))
          :increment ((partial (:increment functions) function) length (:val (first actions)))
          :cut ((partial (:cut functions) function) length (:val (first actions)))
          :stack ((partial (:stack functions) function) length))))))


(defn run [[a b] n length]
  (let [[b c] (compose [a b] [a b] length)])
  (cond
    (= n 1) [a b]
    (= (mod n 2) 0) (run (compose [a b] [a b] length) (quot n 2) length)
    :else (compose [a b] (run (compose [a b] [a b] length) (quot (dec n) 2) length) length)))

(defn part-1 []
  (applyf
    (run
      (actions->function input [(bigint 1) 0] 10007 {:increment increment
                                                     :stack stack
                                                     :cut cut})
      1
      10007)
    2019
    10007))

(defn part-2 []
  (let [length 119315717514047]
    (applyf
      (run
        (actions->function (reverse input) [(bigint 1) 0] length {:increment increment-backward
                                                                  :stack stack-backward
                                                                  :cut cut-backward})
        101741582076661
        length)
      2020
      length)))



; (println (time (part-2)))
