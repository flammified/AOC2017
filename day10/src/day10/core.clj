(ns day10.core
  (:use [clojure.string :as str :refer [split]]))

(def text (slurp "input.txt"))

(defn parse-numbers [numbers]
  (vec (map read-string (str/split numbers #","))))

(defn split-without-wrap [list start length]
  (let [reversed-sub (reverse (take length (drop start (cycle list))))]
    (vec (concat (take start list) reversed-sub (drop (+ start length) list)))))

(defn wraps? [list start length]
  (>= (+ start length) (count list)))

(defn split-with-wrap [list start length]
  (let [reversed-sub (vec (reverse (take length (drop start (cycle list)))))
        sublist-split (- (count reversed-sub) (- (+ start length) (count list)))
        split-reversed (split-at sublist-split reversed-sub)
        back-removed (drop-last (count (first split-reversed)) list)
        front-removed (drop (count (second split-reversed)) back-removed)]
    (vec (concat (vec (second split-reversed)) front-removed (vec (first split-reversed))))))

(defn reverse-sublist [list start length]
  (if (wraps? list start length)
    (split-with-wrap list start length)
    (split-without-wrap list start length)))

(defn wrap-range [new-index max-index]
  (if (>= new-index max-index)
    (- new-index max-index)
    new-index))

(defn knot-list [circle lengths position skip-length]
  (if (== (count lengths) 0)
    circle
    (let [length (first lengths)
          new-index (wrap-range (+ length skip-length position) (count circle))]
      (recur (reverse-sublist circle position length) (drop 1 lengths) new-index (inc skip-length)))))

(defn multiply-first-two [a]
  (* (first a) (second a)))

(defn -main []
  (let [knot-lengths (parse-numbers text)
        output (knot-list (vec (range 0 256)) knot-lengths 0 0)]
    (println output)))
