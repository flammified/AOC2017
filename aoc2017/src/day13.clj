(ns day13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.edn :as edn]))


(defn parse-line [line]
  (let [ [id-str length-str] (str/split line #": ")
         id (edn/read-string id-str)
         length (edn/read-string length-str)]
    {id length}))


(def text
  (-> "day13/input.txt" io/resource io/file slurp))

(def input (->> text
             (str/split-lines)
             (map parse-line)
             (into {})))

(defn caught-at? [idx length delay]
  (= (mod (+ idx delay) (-> length (- 1) (* 2))) 0))

(defn caught [delay firewall]
  (keep
    (fn [[idx length]]
      (when (caught-at? idx length delay) [idx length]))
    firewall))

(defn caught? [delay firewall]
  (reduce
    (fn [caught [idx length]]
      (if (caught-at? idx length delay)
        (reduced true)
        false))
    false
    firewall))


(defn part-1 []
  (let [caught (caught 0 input)]
    (->> caught
         (reduce (fn [sum [idx length]] (+ sum (* idx length))) 0))))

(defn part-2 []

  (->> (range)
       (filter #(not (caught? % input)))
       first))
