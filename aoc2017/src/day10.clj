(ns day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer (cl-format)]))

(def input
  (-> "day10/input.txt" io/resource io/file slurp str/trim))

(def part-2-input "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24")


(def lengths (map read-string (str/split input #",")))

(def start (range 0 256))

(defn pinch [list from length]
  (let [list-length (count list)
        list-remainder-length (- list-length from)
        sublist-at-front (take list-length (drop from (cycle list)))
        [sublist rest] (split-at length sublist-at-front)
        reversed-sublist (reverse sublist)]
     (->> (cycle (concat reversed-sublist rest))
          (drop list-remainder-length)
          (take list-length))))

(defn knothash-round
    ([numbers lengths] (knothash-round numbers lengths 0 0))
    ([numbers lengths stepsize current-position]
     (loop [list numbers
            current-position current-position
            stepsize stepsize
            lengths lengths]
       (let [next-length (first lengths)]
         (if (nil? next-length)
           [current-position stepsize list]
           (recur
               (pinch list current-position next-length)
               (mod (+ current-position next-length stepsize) (count list))
               (inc stepsize)
               (rest lengths)))))))

(def magic [17 31 73 47 23])

(defn create-sparse-hash [input amount-of-loops]
  (let [ascii (concat (map int input) magic)]
    (loop [iterations amount-of-loops
           numbers (range 0 256)
           stepsize 0
           current-position 0]
      (let [[new-position new-stepsize new-list] (knothash-round numbers ascii stepsize current-position)]
        (if (<= iterations 0)
           numbers
           (recur (dec iterations) new-list new-stepsize new-position))))))

(defn reduce-xor [block]
  (reduce bit-xor block))

(defn knothash [input]
  (let [sparse-hash (create-sparse-hash input 64)
        blocks (partition 16 sparse-hash)]
    (->> blocks (map reduce-xor) (map #(cl-format nil "~2,'0x" %)) (str/join ""))))

(defn knothash-decimal [input]
  (let [sparse-hash (create-sparse-hash input 64)
        blocks (partition 16 sparse-hash)]
    (->> blocks (mapv reduce-xor))))




(defn part-1 []
  (->> (knothash-round start lengths)
       last
       (take 2)
       (apply *)))


(defn part-2 []
  (knothash part-2-input))
