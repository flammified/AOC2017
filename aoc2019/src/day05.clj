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

(defn get-value [program i mode]
  (case mode
    :immediate i
    :position (if (< i (count program)) (get-in program [i]) nil)))

(defn split-op [op]
  (let [digits (->> op
                    (iterate #(quot % 10))
                    (take-while pos?)
                    (mapv #(mod % 10))
                    rseq)

        l (map str digits)]

    [(edn/read-string (clojure.string/replace (apply str (take-last 2 l)) #"^0+" "")) (drop 2 (reverse l))]))

(defn run-1 [{:keys [program position] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
        i (get-value program i (case f "1" :immediate :position))
        j (get-value program j (case s "1" :immediate :position))]
    (-> state (assoc-in [:program target] (+ i j)) (assoc-in [:position] (+ position 4)))))

(defn run-2 [{:keys [program position] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
        i (get-value program i (case f "1" :immediate :position))
        j (get-value program j (case s "1" :immediate :position))]
    (-> state (assoc-in [:program target] (* i j)) (assoc-in [:position] (+ position 4)))))

(defn run-5 [{:keys [program position] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
        i (get-value program i (case f "1" :immediate :position))
        j (get-value program j (case s "1" :immediate :position))]
    (if (not (zero? i)) (assoc state :position j) (assoc state :position (+ position 3)))))

(defn run-6 [{:keys [program position] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
        i (get-value program i (case f "1" :immediate :position))
        j (get-value program j (case s "1" :immediate :position))]
    (if (zero? i) (assoc state :position j) (assoc state :position (+ position 3)))))

(defn run-7 [{:keys [program position] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
        i (get-value program i (case f "1" :immediate :position))
        j (get-value program j (case s "1" :immediate :position))]
    (-> state (assoc-in [:program target] (if (< i j) 1 0)) (assoc-in [:position] (+ position 4)))))

(defn run-8 [{:keys [program position] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
          i (get-value program i (case f "1" :immediate :position))
          j (get-value program j (case s "1" :immediate :position))]
      (-> state (assoc-in [:program target] (if (= i j) 1 0)) (assoc-in [:position] (+ position 4)))))

(defn run-3 [{:keys [program position] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
        i-a (get-value program i (case f "1" :immediate :position))
        j (get-value program j (case s "1" :immediate :position))]
    (-> state (assoc-in [:program i] 5) (assoc-in [:position] (+ position 2)))))

(defn run-4 [{:keys [program position] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
        i-a (get-value program i (case f "1" :immediate :position))
        j (get-value program j (case s "1" :immediate :position))]
    (-> state ((fn [state] (do (println "v" (get-in state program [i-a])) state))) (assoc-in [:position] (+ position 2)))))

(defn run-program [{:keys [program position] :as state}]
  (let [[op params] (split-op (get program position))]
    (case op
         99 (assoc state :halted true)
         1 (run-1 state op params)
         2 (run-2 state op params)
         3 (run-3 state op params)
         4 (run-4 state op params)
         5 (run-5 state op params)
         6 (run-6 state op params)
         7 (run-7 state op params)
         8 (run-8 state op params)
         (assoc state :halted true :error true))))

(defn run [program noun verb]
    (-> program
        ((fn [program] (->> (iterate run-program {:halted false :position 0 :program program})
                            (take-while (fn [state] (not (:halted state))))
                            (last)
                            (:halted))))))
(defn part-1 []
  (run input 12 2))

(defn part-2 []
  (filter some?
    (for [noun (range 352)
          verb (range 352)]
        (if (= 19690720 (run input noun verb))
          (+ verb (* 100 noun))
          nil))))

(part-1)
