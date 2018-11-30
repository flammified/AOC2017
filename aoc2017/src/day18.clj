(ns day18
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

(def input
  (-> "day18/input.txt" io/resource io/file slurp))

(spec/def ::arg (spec/or :register simple-symbol? :value int?))
(spec/def ::unary (spec/cat :operation '#{snd rcv} :arg1 ::arg))
(spec/def ::binary (spec/cat :operation '#{set add mul mod jgz} :arg1 ::arg :arg2 ::arg))
(spec/def ::instruction (spec/or :unary ::unary :binary ::binary))


(def instructions
  (->> input
       (str/split-lines)
       (mapv #(spec/conform ::instruction (edn/read-string (format "[%s]" %))))
       (into [])))

(defn step [send receive {:keys [registers count index ] :as state}]
  (if (contains? instructions index)
    (let [[type {:keys [operation arg1 arg2] :as instr}] (instructions index)
          state (update state :index inc)

          value (fn [[kind v]]
                    (if (= kind :register) (registers v) v))

          update-register (fn [update-fn]
                            (update-in state [:registers (second arg1)] (fnil update-fn 0) (value arg2)))]

      (case operation

        set (update-register (fn [a b] b))
        mod (update-register mod)
        add (update-register +)
        mul (-> (update-register *) (update :count inc))
        jgz (cond-> state (pos? (value arg1)) (assoc :index (+ index (value arg2))))

        snd (send state (value arg1) (value arg2))
        rcv (receive state (value arg1) (value arg2))))
    (assoc state :done true)))

(defn part-1 []
  (let [send-function (fn [state sound arg2]
                        (cond-> state (pos? sound) (assoc :last-sound sound)))

        receive-function (fn [state arg1 arg2]
                           (assoc state :recovered (:last-sound state)))]

    (->> (iterate (partial step send-function receive-function) {:registers {} :count 0 :index 0 :done false})
         (some (fn [state] (get state :recovered))))))
         ; :recovered)))

(defn part-1 []
  (let [queue-0 (clojure.lang.PersistentQueue/EMPTY)
        queue-1 (clojure.lang.PersistentQueue/EMPTY)

        send-function (fn [state sound arg2]
                        (let [send-queue (:sendq state)]
                          (conj sound send-queue)))

        receive-function (fn [{:keys [receiveq arg1 arg2] :as state} arg1 arg2]
                           (if-let [sound (peek receiveq)]
                             (-> state
                               (update-in [:registers arg1] sound))))]




    (->> (iterate (partial step send-function receive-function) {:registers {} :count 0 :index 0 :done false})
         (some (fn [state] (get state :recovered))))))
         ; :recovered)))
