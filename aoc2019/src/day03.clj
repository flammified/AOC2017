(ns day03
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.async :refer [>!! <!! to-chan chan]]
            [clojure.math.numeric-tower :as math]))


(defn parse-delta [[dir & length]]
  (let [dir (str dir)
        length (edn/read-string (apply str length))]
    {:direction dir :length length}))

(defn apply-direction [[x y] dir length]
  (case dir
    "R" [(+ x length) y]
    "U" [x (+ y length)]
    "L" [(- x length) y]
    "D" [x (- y length)]))

(defn generate-steps [position direction length]
  (for [step (range 1 (inc length))]
    (apply-direction position direction step)))

(defn create-path [position deltas]
  (reduce
    (fn [{:keys [position path] :as state} {:keys [direction length]}]
      (let [steps (generate-steps position direction length)]
        {:position (last steps) :path (concat path steps)}))
    {:position position :path [[0 0]]}
    deltas))

(defn find-intersections [path1 path2]
  (let [path1 (disj (set path1) [0 0])
        path2 (set path2)]

    (filter #(contains? path2 %) path1)))


(defn manhattan [[x y]]
  (+ (math/abs x) (math/abs y)))

(defn find-closest-intersection [[line1 line2]]
  (let [path1 (:path (create-path [0 0] line1))
        path2 (:path (create-path [0 0] line2))
        intersections (find-intersections path1 path2)]
    (->> intersections
         (map manhattan)
         (reduce min))))

(defn find-intersection-with-least-steps [[line1 line2]]
  (let [path1 (:path (create-path [0 0] line1))
        path2 (:path (create-path [0 0] line2))
        intersections (find-intersections path1 path2)]
    (->> intersections
         (map (fn [p] (+ (.indexOf path1 p) (.indexOf path2 p))))
         (reduce min))))


(def input
  (-> "day03/input.txt"
      io/resource io/file slurp str/trim
      (str/split-lines)
      (->> (map (fn [line] (str/split line #",")))
           (map (fn [line] (map parse-delta line))))))


(defn part-1 []
  (find-closest-intersection input))

(defn part-2 []
  (find-intersection-with-least-steps input))
