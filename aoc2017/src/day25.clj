(ns day25
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

;; Fuck parsing this.

(def input {
              :A [[1 1 :B] [1 -1 :E]]
              :B [[1 1 :C] [1 1 :F]]
              :C [[1 -1 :D] [0 1 :B]]
              :D [[1 1 :E] [0 -1 :C]]
              :E [[1 -1 :A] [0 1 :D]]
              :F [[1 1 :A] [1 1 :C]]})

(defn run [program]
  (loop [index 0
         tape {}
         state :A
         until 12459852]
    (if (= until 0)
      tape
      (let [current-state (state program)
            current-value (or (get tape index) 0)
            [write delta new-state :as branch] (if (pos? current-value) (get current-state 1) (get current-state 0))]
        (recur (+ index delta) (assoc tape index write) new-state (dec until))))))





(defn part-1 []
  (->> (run input)
       (reduce-kv (fn [res idx val] (+ res val)) 0)))
