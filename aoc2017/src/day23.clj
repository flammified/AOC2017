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

(defn update-register [registers update-fn])

(defn step [{:keys [registers count index] :as state}]
  (println registers)
  (let [instruction (get instructions index)
        updated-state (update state :index inc)]
    (if (nil? instruction)
      (assoc updated-state :done true)
      (case (:operation instruction)
            set (assoc-in updated-state  [:registers (second (:arg1 instruction))] (value registers (:arg2 instruction)))
            sub (update-in updated-state  [:registers (second (:arg1 instruction))] #(- % (value registers (:arg2 instruction))))
            mul (-> updated-state
                    (update :count inc)
                    (update-in [:registers (second (:arg1 instruction))] #(* % (value registers (:arg2 instruction)))))
            jnz (cond-> updated-state (not (zero? (value registers (:arg1 instruction)))) (assoc :index (+ index (value registers (:arg2 instruction)))))))))


;
; (defn value [bank arg]
;   (if (#{\a \b \c \d \e \f \g} arg)
;     (get bank arg)
;     arg))
;
; (defn run-line [[op & args] bank]
;   (println (type op))
;   (case op
;     "set" (assoc (args 0) (value (args 1)))))

(defn part-1 []
  (let [registers (into {} (zipmap ['a 'b 'c 'd 'e 'f 'g 'h] (repeat 0)))]
    (last (take-while #(not (:done %)) (iterate step {:registers registers :count 0 :index 0 :done false})))))
;
(defn part-2 []
  (let [registers (-> (into {} (zipmap ['a 'b 'c 'd 'e 'f 'g 'h] (repeat 0)))
                      (assoc 'a 1))]
    (last (take-while #(not (:done %)) (iterate step {:registers registers :count 0 :index 0 :done false})))))
;
