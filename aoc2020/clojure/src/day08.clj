(ns day07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(defn parse [l]
  (let [digit (first (map edn/read-string (re-seq #"-?\d+" l)))
        [op _] (str/split l #" ")]
    {:digit digit
     :op (keyword op)}))




(def input
  (-> "day08/input.txt"
      io/resource
      io/file slurp
      str/trim
      str/split-lines
      (->> (map parse))
      (vec)))

(defn terminates? [program]
  (loop [state {:accumulator 0
                :index 0}
         visited #{}]
    (let [{:keys [op digit action]} (get program (:index state))]
      (if (contains? visited (:index state))
        [false (:accumulator state)]
        (if (= (:index state) (count input))
          [true (:accumulator state)]
          (case op
            :default state
            :nop (recur
                   (update state :index inc)
                   (conj visited (:index state)))
            :jmp (recur
                   (update state :index #(+ digit %))
                   (conj visited (:index state)))
            :acc (recur
                   (-> state
                     (update :accumulator #(+ digit %))
                     (update :index inc))
                   (conj visited (:index state)))))))))


(defn new-prog [ind]
  (let [{:keys [digit action op]} (get input ind)]
    (case op
      :acc input
      :nop (assoc-in input [ind :op] :jmp)
      :jmp (assoc-in input [ind :op] :nop))))


(defn part-1 []
  (second (terminates? input)))

(defn part-2 []
  (loop [index 0]
    (let [prog (new-prog index)
          [good val] (terminates? prog)]
      (if good
        val
        (recur (inc index))))))
