(ns day07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [ubergraph.core :as uber]
            [loom.alg :as alg2]
            [loom.graph :as graph]))


(defn parse-bag [bag]
  (let [parts (str/split bag #" ")]
    [(read-string (first parts)) (str/join " " (rest parts))]))


(defn parse [line]
  (let [line (str/join "" (drop-last line))
        line (str/replace line "bags" "bag")
        [l r] (str/split line #" contain ")
        bags (str/split r #", ")]
    (if (= (first bags) "no other bag")
      []
      (for [[weight bag] (map parse-bag bags)]
        [l bag weight]))))



(def input
  (-> "day07/input.txt"
      io/resource
      io/file slurp
      str/trim
      str/split-lines
      (->>
        (mapcat parse)
        (apply uber/digraph))))


(defn part-1 []
  (count
    (filter
      #(and (not (= % "shiny gold bag")) (alg2/dijkstra-path-dist input % "shiny gold bag"))
      (keys (:node-map input)))))


(defn part-2 []
  (loop [stack '([1 "shiny gold bag"])
         total 0]
    (if (empty? stack)
      total
      (let [[amount node] (peek stack)
            next (graph/out-edges input node)]
        (recur
          (pop stack)
          (reduce
            (fn [total edge]
              (println (:attr edge)))
            next))))))
