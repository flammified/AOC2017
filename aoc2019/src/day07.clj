(ns day06
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.core.async :as async :refer [close! >!! chan <!! go-loop go]]))

(def input
  (-> "day07/input.txt"
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

(defn run-3 [{:keys [id program position input] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
        i-a (get-value program i (case f "1" :immediate :position))
        j (get-value program j (case s "1" :immediate :position))
        in (<!! input)]
    (-> state (assoc-in [:program i] in)
              (assoc-in [:position] (+ position 2)))))

(defn run-4 [{:keys [program position output] :as state} op [f s t]]
  (let [[op i j target] (subvec program position (min (count program) (+ position 4)))
        i-a (get-value program i (case f "1" :immediate :position))
        j (get-value program j (case s "1" :immediate :position))]
    (do
      (>!! output i-a)
      (-> state (assoc-in [:position] (+ position 2))))))

(defn run-program [{:keys [id program position output] :as state}]
  (let [[op params] (split-op (get program position))]
    (case op
         99 (do (close! output)(assoc state :halted true))
         1 (run-1 state op params)
         2 (run-2 state op params)
         3 (run-3 state op params)
         4 (run-4 state op params)
         5 (run-5 state op params)
         6 (run-6 state op params)
         7 (run-7 state op params)
         8 (run-8 state op params)
         (do (close! output) (assoc state :halted true :error true)))))

(defn run-sync [program in-channel out-channel]
  (-> program
      ((fn [program] (->> (iterate run-program {:halted false :position 0 :program program :input in-channel :output out-channel})
                          (take-while (fn [state] (not (:halted state))))
                          (last))))))

(defn run-async [id program]
  (let [in-chan (chan 2)
        out-chan (chan)]
    (go-loop [] (doall (take-while (fn [state] (not (:halted state))) (iterate run-program {:id id :halted false :position 0 :program program :input in-chan :output out-chan}))))
    [in-chan out-chan]))

(defn run-independently [program a b c d e]
  (let [amplifiers {:a (run-amplifier :a program)
                    :b (run-amplifier :b program)
                    :c (run-amplifier :c program)
                    :d (run-amplifier :d program)
                    :e (run-amplifier :e program)}]

    (>!! (-> amplifiers :a first) a)
    (>!! (-> amplifiers :b first) b)
    (>!! (-> amplifiers :c first) c)
    (>!! (-> amplifiers :d first) d)
    (>!! (-> amplifiers :e first) e)

    (try
      (loop [previous 0
             next (cycle [:b :c :d :e :a])]
        (let [[in out] (get amplifiers (first next))]
          (do
            (>!! in previous)
            (let [output (<!! out)]
              (if (nil? output)
                previous
                (recur output (drop 1 next))))))))))

(defn part-1 []
  (apply max (filter some? (for [a (range 0 5)
                                 b (range 0 5)
                                 c (range 0 5)
                                 d (range 0 5)
                                 e (range 0 5)]
                             (let [t (str a b c d e)]
                               (if (= (count (set t)) (count t))
                                 (run-independently input a b c d e)))))))

(defn part-2 []
  (apply max (filter some? (for [a (range 5 10)
                                 b (range 5 10)
                                 c (range 5 10)
                                 d (range 5 10)
                                 e (range 5 10)]
                             (let [t (str a b c d e)]
                               (if (= (count (set t)) (count t))
                                 (run-independently input a b c d e)))))))

(println (part-1))
(println (part-2))
