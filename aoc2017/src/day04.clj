(ns day04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (-> "day4/input.txt" io/resource io/file slurp))

(defn sort-word [word]
  (str (sort (seq (char-array word)))))


(defn valid-2? [passphrase]
  (== 0 (count
          (filter #(> (second %) 1)
            (reduce
              (fn [m k] (update-in m [(sort-word (str/lower-case k))] (fnil inc 0)))
              {}
              (str/split passphrase #" "))))))

(defn valid-1? [passphrase]
  (== 0 (count
          (filter #(> (second %) 1)
            (reduce
              (fn [m k] (update-in m [(str/lower-case k)] (fnil inc 0)))
              {}
              (str/split passphrase #" "))))))

(defn part-1 []
  (count
    (filter
      valid-1?
      (str/split-lines input))))


(defn part-2 []
  (count
    (filter
      valid-2?
      (str/split-lines input))))
