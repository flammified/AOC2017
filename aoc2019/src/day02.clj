(ns day01
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

(def input
  (-> "day02/input.txt"
      io/resource io/file slurp str/trim
      (str/split #",")
      (->> (map edn/read-string) (into []))))

(defn run-op [state [op noun verb target]]
  (let [i (get-in state [:program noun])
        j (get-in state [:program verb])];j])]
    (case op
      99 (assoc state :halted true)
      1 (assoc-in state [:program target] (+ i j))
      2 (assoc-in state [:program target] (* i j))
      :default (assoc state :halted true :error true))))

(defn run-program [{:keys [position program halted] :as state}]
  (if halted
    state
    (let [[op i j target] (subvec program position (+ position 4))
          new-state (run-op state [op i j target])]
      (-> new-state
          (assoc :position (+ position 4))))))

(defn set-position [program position value]
  (assoc program position value))

(defn run-part-1 [program noun verb]
  (try
    (let [program (-> program (set-position 1 noun) (set-position 2 verb))
          initial-state {:halted false :position 0 :program program}]
      (get-in (last (take-while (fn [state] (not (:halted state))) (iterate run-program initial-state))) [:program 0]))
    (catch Exception e
      (str ""))))



(defn part-1 []
  (run-program-1 input 12 2))

(defn part-2 []
  (doseq [noun (range 352)
           verb (range 352)]
      (try
        (if (= 19690720 (run-part-1 input noun verb))
          (println noun verb))
        (catch Exception e
          (str "")))))



(part-2)
