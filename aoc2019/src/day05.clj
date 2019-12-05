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

(defn arity [op] ({1 4 2 4 3 2 4 2 5 3 6 3 7 4 8 4} op))

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

(defmethod run-instruction 3 [{[i j] :arguments} {:keys [program position] :as state}]
  (-> state (assoc-in [:program i] 5)
            (assoc-in [:position] (+ position 2))))

(defmethod run-instruction 4 [{[i] :arguments} {:keys [program position] :as state}]
  (-> state ((fn [state] (do (println "v" i) state)))
            (assoc-in [:position] (+ position 2))))

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

(defn last-can-be-positional? [op]
  ({1 false 2 false 3 false 4 true 5 true 6 true 7 false 8 false} op))

(defn get-arguments [program position op]
  (subvec program position (+ position (arity op))))

(defn run-program [{:keys [program position] :as state}]
  (let [[op params] (split-op (get program position))
        arguments (->> (mapv vector (rest (get-arguments program position op)) (map mode-from-string (concat params (repeat nil))))
                       (#(cond
                          (last-can-be-positional? op) %
                           :else (assoc-in % [(dec (count %)) 1] :immediate)))
                       (mapv #(apply (partial get-value program) %)))]
    (run-instruction {:opcode op :arguments arguments} state)))

(defn run [program]
    (-> program
        ((fn [program] (->> (iterate run-program {:halted false :position 0 :program program})
                            (take-while (fn [state] (not (:halted state))))
                            (last))))))
(defn part-1 []
  (run input))

(part-1)
