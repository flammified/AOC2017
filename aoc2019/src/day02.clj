(ns day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

(def input
  (-> "day02/input.txt"
      io/resource io/file slurp str/trim
      (str/split #",")
      (->> (map edn/read-string) (into []))))

(defn run-program [{:keys [program position] :as state}]
  (let [[op i j target] (subvec program position (+ position 4))
        i (get-in state [:program i])
        j (get-in state [:program j])]
    (-> (case op
         99 (assoc state :halted true)
         1 (assoc-in state [:program target] (+ i j))
         2 (assoc-in state [:program target] (* i j))
         (assoc state :halted true :error true))
       (assoc-in [:position] (+ position 4)))))

(defn run [program noun verb]
  (try
    (-> program
        (assoc 1 noun)
        (assoc 2 verb)
        ((fn [program] (->> (iterate run-program {:halted false :position 0 :program program})
                            (take-while (fn [state] (not (:halted state))))
                            (last)
                            ((fn [state] (get-in state [:program 0])))))))
    (catch Exception e
      (str ""))))

(defn part-1 []
  (run input 12 2))

(defn part-2 []
  (filter some?
    (for [noun (range 352)
          verb (range 352)]
        (if (= 19690720 (run input noun verb))
          (+ verb (* 100 noun))
          nil))))
