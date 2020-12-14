(ns day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [util.grids :as grids]
            [clojure.math.numeric-tower :as math]))

(defn parse-line [line]
  (let [[left right] (str/split line #" = ")
        address (first (map edn/read-string (re-seq #"-?\d+" left)))
        val (first (map edn/read-string (re-seq #"-?\d+" right)))]
    [address val]))

(defn parse-mask [mask]
  (let [[left right] (str/split mask #" = ")]
    right))


(defn set-mask [val mask]
  (reduce-kv
    (fn [val k c]
      (case c
        \X val
        \0 (bit-clear val k)
        \1 (bit-set val k)
        val))
    val
    (vec (reverse mask))))


(defn set-memory [mem [address val]  mask]
  (assoc mem address (set-mask val mask)))

(defn run [lines]
  (reduce
    (fn [{:keys [mask mem]} line]
      (case (str/includes? line "mask")
        true {:mask (parse-mask line) :mem mem}
        false {:mask mask :mem (set-memory mem (parse-line line) mask)}))
    {:mask ""
     :mem {}}
    lines))

(defn mask-to-possible-vals [address mask]
  (reduce-kv
    (fn [vals k c]
      (case c
        \X (reduce-kv (fn [vals i cur-val]
                        (-> vals
                            (assoc i (bit-set cur-val k))
                            (conj (bit-clear cur-val k))))
             vals
             vals)
        \0 (reduce-kv (fn [vals i cur-val]
                        (assoc vals i (case (bit-test address k)
                                            true (bit-set cur-val k)
                                            false (bit-clear cur-val k))))
             vals
             vals)
        \1 (reduce-kv (fn [vals i cur-val]
                        (assoc vals i (bit-set cur-val k)))
             vals
             vals)
        val))
    [0]
    (vec (reverse mask))))


(defn run2 [lines]
  (reduce
    (fn [{:keys [mask mem]} line]
      (case (str/includes? line "mask")
        true {:mask (parse-mask line) :mem mem}
        false
          (let [[address val] (parse-line line)]
            {:mask mask
             :mem  (reduce
                     (fn [mem address]
                       (assoc mem address val))
                     mem
                     (mask-to-possible-vals address mask))})))

    {:mask ""
     :mem {}}
    lines))

(def input
  (-> "day14/input.txt"
      io/resource
      io/file slurp
      str/trim
      str/split-lines))


(defn part-1 []
  (println (reduce + (vals (:mem (run input))))))

(defn part-2 []
  (println (reduce + (vals (:mem (run2 input))))))

(part-1)
