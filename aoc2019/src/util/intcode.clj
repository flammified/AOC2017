(ns util.intcode
  (:require [clojure.core.async :as async :refer [chan go-loop close! >!! <!! poll!]]
            [clojure.edn :as edn]
            [clojure.string :as str]))


(defn arity [op] ({1 4 2 4 3 2 4 2 5 3 6 3 7 4 8 4 9 2 99 0} op))

(defn parse-program [text]
  (-> text
      str/trim
      (str/split #",")
      (->> (map edn/read-string) (into []))))


(defn get-memory [state i]
  (if (>= i (count (:program state)))
    (let [v (get-in state [:extra i])]
      (if (nil? v) 0 v))
    (get-in state [:program i])))


(defn get-value [state [i mode]]
  (case mode
    :relative (get-memory state (+ i (:relative state)))
    :immediate i
    :position (get-memory state i)))

(defn set-memory [state i v]
  (if (> i (count (:program state)))
    (assoc-in state [:extra i] v)
    (assoc-in state [:program i] v)))

(defn set-value [state [i mode] value]
  (case mode
    :relative (set-memory state (+ i (:relative state)) value)
    :position (set-memory state i value)))

(defn split-op [op]
  (let [digits (->> op
                    (iterate #(quot % 10))
                    (take-while pos?)
                    (mapv #(mod % 10))
                    rseq)
        l (map str digits)]

    [(edn/read-string (clojure.string/replace (apply str (take-last 2 l)) #"^0+" ""))
     (map str (drop 2 (reverse l)))]))

(defn mode-from-string [m]
  (case m
      "2" :relative
      "1" :immediate
      "0" :position
      :position))

(defn add [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state
      (set-value target (+ (get-value state i) (get-value state j)))
      (assoc-in [:position] (+ position 4))))

(defn multiply [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state
      (set-value target (* (get-value state i) (get-value state j)))
      (assoc-in [:position] (+ position 4))))

(defn get-input [{[i] :arguments} {:keys [id program position input id async?] :as state}]
  (let [inp (if async?
                  (<!! input)
                  (first input))]
    (if (some? inp)
      (-> state (set-value i inp)
                (assoc-in [:position] (+ position 2))
                (#(if (not async?) (update % :input (fn [i] (vec (drop 1 i)))) %)))
      (-> state (set-value i -1)
                (assoc-in [:position] (+ position 2))
                (assoc :idle true)))))

(defn send-output [{[i] :arguments} {:keys [id program position output async?] :as state}]
  (if async?
    (do
      (>!! output (get-value state i))
      (-> state (assoc-in [:position] (+ position 2))))
    (-> state (update :output conj (get-value state i))
              (assoc-in [:position] (+ position 2)))))

(defn jump-if-not-zero [{[i j] :arguments} {:keys [program position] :as state}]
  (if (not (zero? (get-value state i)))
      (assoc state :position (get-value state j))
      (assoc state :position (+ position 3))))

(defn jump-if-zero [{[i j] :arguments} {:keys [program position] :as state}]
  (if (zero? (get-value state i))
      (assoc state :position (get-value state j))
      (assoc state :position (+ position 3))))

(defn less-than [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state
      (set-value target (if (< (get-value state i) (get-value state j)) 1 0))
      (assoc-in [:position] (+ position 4))))

(defn equal-to [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state
      (set-value target (if (= (get-value state i) (get-value state j)) 1 0))
      (assoc-in [:position] (+ position 4))))

(defn set-offset [{[i] :arguments} {:keys [program position relative] :as state}]
  (-> state
      (assoc-in [:relative] (+ (get-value state i) relative))
      (assoc-in [:position] (+ position 2))))


(def instructions {1 add
                   2 multiply
                   3 get-input
                   4 send-output
                   5 jump-if-not-zero
                   6 jump-if-zero
                   7 less-than
                   8 equal-to
                   9 set-offset})

(defn get-arguments [state position op]
  (subvec (:program state) position (+ position (arity op))))

(defn run-program [{:keys [id async? program position output] :as state}]
  (if (and (not (:halted state))
           (not (:idle state))
           (not (= (get-memory state position) 99)))
    (let [[op raw] (split-op (get program position))
          arguments (mapv vector (rest (get-arguments state position op)) (map mode-from-string (concat raw (repeat nil))))]
      ((get instructions op) {:opcode op :arguments arguments} state))
    (do
      (if (and async? (= (get-memory state position) 99)) (close! output))
      (assoc state :halted true))))

(defn run-state [state]
  (->> (iterate run-program state)
       (drop-while (fn [state] (and (not (:idle state)) (not (:halted state)))))
       (first)))

(defn run-sync [program initial-input]
  (->> (iterate run-program {:halted false
                             :idle false
                             :relative 0
                             :position 0
                             :program program
                             :extra {}
                             :input initial-input
                             :output []})
       (drop-while (fn [state] (and (not (:idle state)) (not (:halted state)))))
       (first)))

(defn run-async [id program]
  (let [in-chan (chan 100)
        out-chan (chan 100)]
    (go-loop []
      (doall
        (take-while (fn [state]
                      (and (not (:idle state))
                           (not (:halted state))))
                    (iterate run-program {:async? true
                                          :halted false
                                          :idle false
                                          :position 0
                                          :relative 0
                                          :program program
                                          :extra {}
                                          :input in-chan
                                          :output out-chan}))))
    [in-chan out-chan]))
