(ns day18
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def text
  (-> "day18/input.txt" io/resource io/file slurp))

(defn get-register-by-name [bank name]
  (let [name-value (get bank name)]
    (if (nil? name-value)
      0
      name-value)))

(defn highest-register [state]
  (let [highest-key (key (apply max-key val state))]
    [highest-key (get state highest-key)]))

(defn parse-instruction [line]
  (let [[r1 command v1 _ r2 operator v2] (str/split line #" ")]
    {:r1 r1
     :command command
     :v1 (read-string v1)
     :r2 r2
     :operator operator
     :v2 (read-string v2)}))


;; Good excuse to learn multimethods
(defmulti run-operator (fn [bank instruction] (:operator instruction)))

(defmethod run-operator "==" [bank instruction]
  (== (get-register-by-name bank (:r2 instruction)) (:v2 instruction)))

(defmethod run-operator ">" [bank instruction]
  (> (get-register-by-name bank (:r2 instruction)) (:v2 instruction)))

(defmethod run-operator "<" [bank instruction]
  (< (get-register-by-name bank (:r2 instruction)) (:v2 instruction)))

(defmethod run-operator ">=" [bank instruction]
  (>= (get-register-by-name bank (:r2 instruction)) (:v2 instruction)))

(defmethod run-operator "<=" [bank instruction]
  (<= (get-register-by-name bank (:r2 instruction)) (:v2 instruction)))

(defmethod run-operator "!=" [bank instruction]
  (not (== (get-register-by-name bank (:r2 instruction)) (:v2 instruction))))

(defmulti run-command (fn [bank instruction] (:command instruction)))

(defmethod run-command "inc" [bank instruction]
  (let [v1 (:v1 instruction)
        new-bank (update-in bank [(:r1 instruction)] (fnil #(+ % v1) 0))]
    new-bank))

(defmethod run-command "dec" [bank instruction]
  (let [v1 (:v1 instruction)]
    (update-in bank [(:r1 instruction)] (fnil #(- % v1) 0))))

(defn last-state [states]
  (if (nil? (last states))
    {}
    (last states)))

(defn run-instruction [states instruction]
  (let [last-state (last-state states)
        condition-passed (run-operator last-state instruction)]
    (if condition-passed
      (conj states (run-command last-state instruction))
      (conj states last-state))))

(defn run-program [input]
  (->> input
    (str/split-lines)
    (map parse-instruction)
    (reduce run-instruction [])))

(defn highest-ever-state [states]
  (reduce max (map (fn [state] (second (highest-register state))) states)))

(defn part-1 [])
  ; (let [states (run-program text)]
    ; (apply max-key val (last states)))

(defn part-2 [])
  ; (let [states (run-program text)
        ; highest (highest-register (last states))
    ; (highest-ever-state/ states))
