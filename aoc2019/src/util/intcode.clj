(ns util.intcode
  (:require [clojure.core.async :refer [chan go-loop close! >!! <!!]]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;; Refactoring @Todo
;; 1) Move modes for writing to {:type <> :val <>} object
;; 2) Add run-sync function that actually stops on input needed.

(defn new-program [synchronous? program in out]
  {:sync synchronous?
   :halted false
   :relative 0
   :position 0
   :program program
   :extra {}
   :input in
   :output out})

(defn parse-program [text]
  (-> text
      str/trim
      (str/split #",")
      (->> (map edn/read-string) (into []))))

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

(defn set-value [program state i val mode]
  (case mode
    :relative (set-memory state (+ i (:relative state)))
    :position (set-memory state i)))

(defn split-op [op]
  (let [digits (str op)]
    [(->> digits
          (take-last 2)
          (apply str)
          (clojure.string/replace  #"^0+" "")
          (edn/read-string))
     (->> digits
          (reverse)
          (drop 2)
          (map str))]))

(defmulti run-instruction :opcode)

(defmethod run-instruction 1 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state (assoc-in [:position] (+ position 4))
            (set-memory target (+ i j))))

(defmethod run-instruction 2 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state (set-memory target (* i j))
            (assoc-in [:position] (+ position 4))))

(defmethod run-instruction 3 [{[i] :arguments} {:keys [program position input] :as state}]
  (let [inp (<!! input)]
    (-> state (set-memory i inp)
              (assoc-in [:position] (+ position 2)))))

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
      (set-memory target (if (< i j) 1 0))
      (assoc-in [:position] (+ position 4))))

(defmethod run-instruction 8 [{[i j target] :arguments} {:keys [program position] :as state}]
  (-> state
      (set-memory target (if (= i j) 1 0))
      (assoc-in [:position] (+ position 4))))

(defmethod run-instruction 9 [{[i] :arguments} {:keys [program position relative] :as state}]
  (-> state
      (assoc-in [:relative] (+ i relative))
      (assoc-in [:position] (+ position 2))))

(defmethod run-instruction 99 [{:keys [program position] :as state}]
  (-> state
      (assoc-in [:halted] true)))

(defn last-can-be-positional? [op]
  ({1 false 2 false 3 false 4 true 5 true 6 true 7 false 8 false 9 true 99 true} op))

(defn arity [op] ({1 4 2 4 3 2 4 2 5 3 6 3 7 4 8 4 9 2 99 0} op))

(defn get-arguments [state position op]
  (reduce
    #(conj %1 (get-memory state (+ position %2)))
    []
    (range (arity op))))

(defn run-program [{:keys [program position output relative] :as state}]
  (if (not (= (get-memory state position) 99))
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
      (close! output)
      (assoc state :halted true))))

(defn run-sync
  ([program input]
   (run-sync (new-program true program input [])))
  ([program input state]
   (->> (iterate run-program state)
        (take-while (fn [state] (and (not (:input-needed state) (not (:halted state)))))))))

(defn run-async [id program]
  (let [in-chan (chan 5)
        out-chan (chan 5)]
    (go-loop [] (doall
                  (->> (new-program false program in-chan out-chan)
                       (iterate run-program)
                       (take-while (fn [state] (not (:halted state)))))))


    [in-chan out-chan]))
