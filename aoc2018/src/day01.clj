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
          (if (get cur new-frequency)
            (reduced new-frequency)
            (assoc cur :last-frequency new-frequency
                        new-frequency :seen))))
    {:last-frequency 0 0 :seen}
    (cycle input)))
