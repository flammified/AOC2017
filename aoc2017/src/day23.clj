(ns day23
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

(def input
  (-> "day23/input.txt" io/resource io/file slurp))

(spec/def ::arg (spec/or :register simple-symbol? :value int?))
(spec/def ::instruction (spec/cat :operation '#{set sub mul jnz} :arg1 ::arg :arg2 ::arg))


(def instructions
  (->> input
       (str/split-lines)
       (mapv #(spec/conform ::instruction (edn/read-string (format "[%s]" %))))
       (into [])))

(defn value [registers [kind value]]
  (if (= kind :register)
    (registers value)
    value))

(defn step [{:keys [registers count index] :as state}]
  (if (contains? instructions index)
    (let [{:keys [operation arg1 arg2]} (instructions index)
          state (update state :index inc)

          update-register (fn [update-fn]
                            (update-in state [:registers (second arg1)] update-fn (value registers arg2)))]

      (case operation
            set (update-register (fn [a b] b))
            sub (update-register -)
            mul (-> (update-register *) (update :count inc))
            jnz (cond-> state (not (zero? (value registers arg1))) (assoc :index (+ index (value registers arg2))))))
    (assoc state :done true)))

(defn part-1 []
  (let [registers (into {} (zipmap ['a 'b 'c 'd 'e 'f 'g 'h] (repeat 0)))]
    (->> (take-while #(not (:done %)) (iterate step {:registers registers :count 0 :index 0 :done false}))
         last
         :count)))

(defn prime? [num]
  (let [p (fn [x] (== (mod num x) 0))]
      (not (some p (range 2 num)))))

(defn part-2 []
  (->> (range 105700 (+ 122700 1), 17)
      (filter #(not (prime? %)))
      count))
