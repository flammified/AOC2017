(ns day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.pprint :refer (cl-format)]))

(def input
  (-> "day19/input.txt" io/resource io/file slurp))

(def matrix (mapv vec (str/split-lines input)))

(def letters (set (map (comp char) (range (int \A) (int \Z)))))

(defn direction-to-coords [direction]
  (case direction
    :left       [0 -1]
    :right      [0 1]
    :down       [1 0]
    :up         [-1 0]
    :default    [0 0]))

(defn move [state]
    (update-in state [:coord] #(map + % (direction-to-coords (:direction state)))))

(defn update-direction [state]
  (let [[iy ix] (:coord state)]
    (assoc state :direction
      (case (get-in matrix (:coord state))
        \+ (case (:direction state)
              (:up :down) (if (#{\| \space} (get-in matrix [iy (dec ix)])) :right :left)
              (:left :right) (if (#{\- \space} (get-in matrix [(dec iy) ix])) :down :up))
        (:direction state)))))


(defn path [state]
  (loop [state state
         states []]

    (let [new-state (-> state
                        update-direction
                        move)]

      (if (#{\space} (get-in matrix (:coord state)))
        states
        (recur new-state (conj states state))))))


(defn start-location [matrix]
  (let [first-row (first matrix)]
    [0 (.indexOf first-row \|)]))

(defn part-1 []
  (->> (path {:coord (start-location matrix) :direction :down})
       (map #(get-in matrix (:coord %)))
       (filter (fn [character] (contains? letters character)))
       (str/join "")))

(defn part-2 []
  (->> (path {:coord (start-location matrix) :direction :down})
       (count)))
