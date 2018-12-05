(ns day05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; We'll request to send `profile` stats to `println`:
(tufte/add-basic-println-handler! {})


(def input
  (-> "day05/input.txt"
      io/resource io/file slurp str/trim))

(def lowercase (map char (range 97 123)))
(def uppercase (map char (range 65 91)))

(def lettermap (merge (zipmap uppercase lowercase) (zipmap lowercase uppercase)))
(def elements (distinct (map str/lower-case input)))

(defn reacts? [a b]
  (= (lettermap a) b))

(defn react [letters]
  (loop [tail letters
         polymer []]
    (let [[current next-element] (take 2 tail)]
      (if (p :empty (empty? tail))
        polymer
        (if (p :reacts (reacts? current next-element))
            (recur (p :drop (drop 2 tail)) polymer)
            (recur (p :drop2 (drop 1 tail)) (conj polymer current)))))))

(defn react-until-end [string]
  (loop [polymer (vec (seq string))]
    (let [new-polymer (react polymer)]
      (if (= new-polymer polymer)
        (str/join "" polymer)
        (recur new-polymer)))))

(defn part-1 []
  (profile {} (-> input
                  (react-until-end)
                  count)))

(defn part-2 [])
