(ns day25
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-sync run-state]]
            [util.grids :as grids :refer [draw-sparse]]))


(def input
  (-> "day25/input.txt"
      io/resource io/file slurp str/trim
      (intcode/parse-program)))

(defn write [state val]
  (let [state (assoc-in state [:input] (vec (map #(int %) val)))]
    (run-state state)))

(defn directions [text]
  (filter some?
    (for [direction ["north" "east" "south" "west"]]
      (if (contains? text direction) direction))))

(defn run []
  (let [state (run-sync input [])]
        ; queue
    (loop [state state
           ; inputs
           inputs ["north\n"
                   "north\n"
                   "take astrolabe\n" "south\n" "south\n"
                   "west\n" "north\n" "east\n" "take space law space brochure\n"
                   "west\n" "south\n" "west\n" "east\n" "east\n" "east\n"
                   "west\n" "north\n" "north\n" "take prime number\n"
                   "south\n" "south\n" "east\n"
                   "south\n" "south\n" "west\n" "east\n" "west\n" "take mouse\n" "north\n" "north\n" "east\n"]]
      (let [output (get-in state [:output])
            state (-> state
                      (assoc-in [:output] [])
                      (assoc-in [:idle] false))]
          (let [directions (directions output)]
            (println (str/join (map char output)))
            (if (empty? inputs)
              nil
              (recur (write state (first inputs)) (drop 1 inputs))))))))


(defn part-1 []
  (run))


; (println (part-1))
