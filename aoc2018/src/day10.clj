(ns day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [lanterna.screen :as screen]))

;#1256 @ 277,6: 29x20

(def scr (screen/get-screen :swing))
(screen/start scr)

(def text
  (-> "day10/input.txt" io/resource io/file slurp))

(def input
  (->> text
       (str/split-lines)
       (map #(map edn/read-string (re-seq #"-?\d+" %)))
       (map #(zipmap [:position :velocity] (partition 2 %)))))


(defn step [particles]
  (map
    (fn [particle]
      (update-in particle [:position] #(mapv + % (:velocity particle))))
    particles))

(defn render [particles]
  (screen/clear scr)
  (doall (map (fn [particle] (let [[x y] (:position particle)] (screen/put-string scr (+ x 10) (+ y 10) "X"))) particles))
  (screen/redraw scr)
  (screen/get-key-blocking scr)
  particles)

(defn y-difference [particles])


(defn part-1 []
  (reduce
    (fn [highest-y-difference new-state]
      (if (> (y-difference new-state) highest-y-difference)
        (reduced new-state))
      (iterate (comp step render) input))))
  
    ; (while true (render particles))))
