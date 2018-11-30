(ns day24
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

(def text
  (-> "day24/input.txt" io/resource io/file slurp))

(def input (->> text
                (str/split-lines)
                (map #(str/split % #"/"))
                (mapv #(mapv edn/read-string %))))

(defn bridges [parts stack pin]
  (let [matches (filter (fn [part] (some #( #{pin} % ) part)) parts)]
    (if
      (empty? matches) [stack]
      (reduce
        (fn [result [s1 s2 :as match]]
          (concat result (bridges (remove #{match} parts) (conj stack match) (if (= s1 pin) s2 s1))))

        []
        matches))))



(defn part-1 []
  (->> (bridges input [] 0)
       (map #(reduce + (map (fn [list] (apply + list)) %)))
       (apply max)))

(defn part-2 []
  (->> (bridges input [] 0)
       (map (fn [bridge] {:count (count bridge) :strength (reduce + (map (fn [list] (apply + list)) bridge))}))
       (sort-by (juxt :count :strength))
       last))
