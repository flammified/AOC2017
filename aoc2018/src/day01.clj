(ns day01
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

(def input
  (-> "day01/input.txt"
      io/resource io/file slurp
      (->>
        (str/split-lines)
        (map edn/read-string))))

(defn part-1 []
  (reduce + input))
(defn part-2 []
  (reduce
      (fn [{:keys [last-frequency] :as cur} delta]
        (let [new-frequency (+ last-frequency delta)]
          (if (some? (get cur new-frequency))
            (reduced new-frequency)
            (-> cur
              (assoc :last-frequency new-frequency)
              (assoc new-frequency :seen)))))
    {:last-frequency 0 0 :seen}
    (cycle input)))
