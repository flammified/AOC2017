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


(defn get-value [program state i mode]
  (case mode
    :relative (get-memory state (+ i (:relative state)))
    :immediate i
    :position (get-memory state i)))

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
      "2" :relative
      "1" :immediate
      "0" :position
      :position))

(defn set-memory [state i v]
  (if (> i (count (:program state)))
    (assoc-in state [:extra i] v)
    (assoc-in state [:program i] v)))

(defmulti run-instruction :opcode)

(defmethod run-instruction 1 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state (assoc-in [:position] (+ position 4))
            (set-memory target (+ i j))))

(defmethod run-instruction 2 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state (set-memory target (* i j))
            (assoc-in [:position] (+ position 4))))

(defmethod run-instruction 3 [{[i] :arguments} {:keys [id program position input id async?] :as state}]
  (let [inp (if async?
                  (<!! input)
                  (first input))]
    (if (some? inp)
      (-> state (set-memory i inp)
                (assoc-in [:position] (+ position 2))
                (#(if (not async?) (update % :input (fn [i] (vec (drop 1 i)))) %)))
      (-> state (set-memory i -1)
                (assoc-in [:position] (+ position 2))
                (assoc :idle true)))))


(defmethod run-instruction 4 [{[i] :arguments} {:keys [id program position output async?] :as state}]
  (spit (str "output-" id ".txt") (str output "\n") :append true)
  (if async?
    (do
      (>!! output i)
      (-> state (assoc-in [:position] (+ position 2))))
    (-> state (assoc-in [:position] (+ position 2))
              (update :output conj i))))

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
      (set-memory target (if (< i j) 1 0))
      (assoc-in [:position] (+ position 4))))

(defmethod run-instruction 8 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state
      (set-memory target (if (= i j) 1 0))
      (assoc-in [:position] (+ position 4))))

(defmethod run-instruction 99 [{:keys [program position] :as state}]
  (-> state
      (assoc-in [:halted] true)))

(defmethod run-instruction 9 [{[i] :arguments} {:keys [program position relative] :as state}]
  (-> state
      (assoc-in [:relative] (+ i relative))
      (assoc-in [:position] (+ position 2))))

(defn last-can-be-positional? [op]
  ({1 false 2 false 3 false 4 true 5 true 6 true 7 false 8 false 9 true 99 true} op))

(defn get-arguments [state position op]
  (reduce
    #(conj %1 (get-memory state (+ position %2)))
    []
    (range (arity op))))

(defn run-program [{:keys [id async? program position output input relative extra] :as state}]
  (if (and (not (:halted state))
           (not (:idle state))
           (not (= (get-memory state position) 99)))
    (let [[op params] (split-op (get program position))
          arguments (->> (mapv vector (rest (get-arguments state position op)) (map mode-from-string (concat params (repeat nil))))
                         (#(cond
                            (last-can-be-positional? op) %
                            :else
                               (let [mode (get-in % [(dec (count %)) 1])]
                                 (case mode
                                   :position (assoc-in % [(dec (count %)) 1] :immediate)
                                   :relative (update % (dec (count %)) (fn [[val b]] [(+ relative val) :immediate]))
                                   %))))
                         (mapv #(apply (partial get-value program state) %)))]
      (run-instruction {:opcode op :arguments arguments} state))
    (do
      (if (and async? (= (get-memory state position) 99)) (close! output))
      (assoc state :halted true))))

(defn run-state [state]
  (->> (iterate run-program state)
       (drop-while (fn [state] (and (not (:idle state)) (not (:halted state)))))
       (first)))

(defn run-sync [program initial-input]
  (->> (iterate run-program {:async? false
                             :halted false
                             :idle false
                             :idle-counter 0
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
