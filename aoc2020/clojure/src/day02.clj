(ns day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))



(defn xor [a b]
  (cond
    (and a b) false
    (and (not a) b) true
    (and (not b) a) true
    (and (not b) (not a)) false))



(defn parse [line]
  (let [[rule password] (str/split line #":")
        [limit letter] (str/split rule #" ")
        [start end] (str/split limit #"-")]
    {:password (str/trim password)
     :first (Integer/parseInt start)
     :end (Integer/parseInt end)
     :letter (get letter 0)}))

(def input
  (-> "day02/input.txt"
      io/resource io/file slurp str/trim
      (str/split-lines)
      (->> (map parse))))

(defn valid-1? [line]
  (let [count (frequencies (:password line))
        letter-count (or (get count (:letter line)) 0)]
    (and
      (>= letter-count (:first line))
      (<= letter-count (:end line)))))

(defn valid-2? [line]
  (xor
    (= (:letter line) (get (:password line) (dec (:first line))))
    (= (:letter line) (get (:password line) (dec (:end line))))))

(defn part-1 []
  (count (filter valid-1? input)))


(defn part-2 []
  (count (filter valid-2? input)))
