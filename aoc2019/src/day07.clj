(ns day07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.core.async :refer [>!! <!! chan go-loop close!]]))

(def input
  (-> "day07/input.txt"
      io/resource io/file slurp str/trim
      (str/split #",")
      (->> (map edn/read-string) (into []))))

(defn arity [op] ({1 4 2 4 3 2 4 2 5 3 6 3 7 4 8 4 99 0} op))

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

    [(edn/read-string (clojure.string/replace (apply str (take-last 2 l)) #"^0+" "")) (map str (drop 2 (reverse l)))]))

(defn mode-from-string [m]
  (case m
      "1" :immediate
      "0" :position
      :position))

(defmulti run-instruction :opcode)

(defmethod run-instruction 1 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state (assoc-in [:position] (+ position 4))
            (assoc-in [:program target] (+ i j))))

(defmethod run-instruction 2 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state (assoc-in [:program target] (* i j))
            (assoc-in [:position] (+ position 4))))

(defmethod run-instruction 3 [{[i j] :arguments} {:keys [program position input] :as state}]
  (-> state (assoc-in [:program i] (<!! input))
            (assoc-in [:position] (+ position 2))))

(defmethod run-instruction 4 [{[i] :arguments} {:keys [program position output] :as state}]
  (do
    (>!! output i)
    (-> state (assoc-in [:position] (+ position 2)))))

(defmethod run-instruction 5 [{[i j] :arguments} {:keys [program position] :as state}]
  (if (not (zero? i))
      (assoc state :position j)
      (assoc state :position (+ position 3))))

(defmethod run-instruction 6 [{[i j] :arguments} {:keys [program position] :as state}]
  (if (zero? i)
      (assoc state :position j)
      (assoc state :position (+ position 3))))

(defmethod run-instruction 7 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state
      (assoc-in [:program target] (if (< i j) 1 0))
      (assoc-in [:position] (+ position 4))))

(defmethod run-instruction 8 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state
      (assoc-in [:program target] (if (= i j) 1 0))

      (assoc-in [:position] (+ position 4))))

(defmethod run-instruction 99 [{:keys [program position] :as state}]
  (-> state
      (assoc-in [:halted] true)))

(defn last-can-be-positional? [op]
  ({1 false 2 false 3 false 4 true 5 true 6 true 7 false 8 false 99 true} op))


(defn get-arguments [program position op]
  (subvec program position (+ position (arity op))))


(defn run-program [{:keys [program position output] :as state}]
  (if (not (= (get program position) 99))
    (let [[op params] (split-op (get program position))
          arguments (->> (mapv vector (rest (get-arguments program position op)) (map mode-from-string (concat params (repeat nil))))
                         (#(cond
                            (last-can-be-positional? op) %
                             :else (assoc-in % [(dec (count %)) 1] :immediate)))
                         (mapv #(apply (partial get-value program) %)))]
      (run-instruction {:opcode op :arguments arguments} state))
    (do
      (close! output)
      (assoc state :halted true))))

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
  (let [amplifiers {:a (run-async :a program)
                    :b (run-async :b program)
                    :c (run-async :c program)
                    :d (run-async :d program)
                    :e (run-async :e program)}]

    (>!! (-> amplifiers :a first) a)
    (>!! (-> amplifiers :b first) b)
    (>!! (-> amplifiers :c first) c)
    (>!! (-> amplifiers :d first) d)
    (>!! (-> amplifiers :e first) e)

    (try
      (loop [previous 0
             next (cycle [:a :b :c :d :e])]

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
