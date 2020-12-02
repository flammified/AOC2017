(ns day14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn])

  (:use [loom.graph]
        [loom.alg]
        [loom.attr]))


(defn add-to-graph [graph line]
  (let [[from to] (str/split line #" => ")
        reqs (str/split from #", ")
        [amount name] (str/split to #" ")]
    (reduce
      (fn [graph req]
        (let [[req-amount req-name] (str/split req #" ")]
          (-> graph
              (add-edges [name req-name])
              (add-attr-to-edges :amount (edn/read-string req-amount) [[name req-name]])
              (add-attr name :produces (edn/read-string amount)))))
      (-> graph
          (add-nodes name)
          (add-attr name :amount amount))
      reqs)))

(def input
  (reduce
    add-to-graph
    (add-nodes (digraph) "FUEL")
    (-> "day14/input.txt"
        io/resource io/file slurp str/trim str/split-lines)))


(defn amount-of-ore [system fuel]
  (get
    (reduce
      (fn [amounts chemical]
        (let [produces (attr system chemical :produces)
              needed (get amounts chemical)]
          (reduce
            (fn [amounts requirement]
              (let [req-amount (attr system chemical requirement :amount)
                    runs (Math/ceil (/ needed produces))]
                (update amounts requirement (fnil + 0) (* runs req-amount))))
            amounts
            (successors system chemical))))

      {"FUEL" fuel}
      (topsort input))
    "ORE"))

(defn binary-search [f target start end]
  (let [pivot (int (/ (+ start end) 2))
        value (f pivot)]
    (if (= (inc start) end)
      pivot
      (cond
        (> value target) (recur f target start pivot)
        (< value target) (recur f target pivot end)
        :else pivot))))

(defn part-1 []
  (amount-of-ore input 1))

(defn part-2 []
  (binary-search (partial amount-of-ore input) 1000000000000 0 5076490))
