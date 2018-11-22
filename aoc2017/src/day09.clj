(ns day09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def text
  (-> "day9/input.txt" io/resource io/file slurp))

(defn create-state []
  {:total-score 0
   :current-score 0
   :ignore false
   :in-garbage false
   :garbage-count 0})

(defn increase-score [state]
  (let [increased-current-score (update-in state [:current-score] inc)]
    (update-in increased-current-score [:total-score] + (:current-score increased-current-score))))

(defn parse-character [state char]
  (let [c (str char)]
    (if
      (not (:ignore state))
      (if
        (:in-garbage state)
        (case c
          "!" (assoc state :ignore true)
          ">" (assoc state :in-garbage false)
          (update-in state [:garbage-count] inc))
        (case c
          "{" (increase-score state)
          "}" (update-in state [:current-score] dec)
          "!" (assoc state :ignore true)
          "<" (assoc state :in-garbage true)
          ">" (assoc state :in-garbage false)
          state))
      (assoc state :ignore false))))

(defn parse-input [input]
  (reduce parse-character (create-state) input))

(defn part-1 []
  (let [output (parse-input text)]
    (:total-score output))) 
(defn part-2 []
  (let [output (parse-input text)]
    (:garbage-count output)))
