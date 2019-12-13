(ns day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))

(def text
  (-> "day12/input.txt" io/resource io/file slurp))

(def input
  (->> text
       (str/split-lines)
       (map #(map edn/read-string (re-seq #"-?\d+" %)))
       (map #(zipmap [:x :y :z] %))
       (mapv #(-> %
                  (assoc :velocity [0 0 0])
                  (assoc :position [(:x %) (:y %) (:z %)])
                  (dissoc :x)
                  (dissoc :y)
                  (dissoc :z)))))

(defn generate-pairs [l]
  (->> (for [i (range l)
             j (range l)]
         [i j])
       (map sort)
       (distinct)
       (map vec)
       (filter (fn [[a b]] (not (= a b))))
       (into [])))


(defn gravity [dim moons]
  (reduce
    (fn [moons [idx-1 idx-2]]
      (let [pivot (get moons idx-1)
            check (get moons idx-2)
            pos (:position pivot)
            pos2 (:position check)]
        (-> moons
            ((fn [r]
               (cond
                 (> (get pos dim) (get pos2 dim)) (-> r
                                                      (update-in [idx-1 :velocity dim] dec)
                                                      (update-in [idx-2 :velocity dim] inc))
                 (< (get pos dim) (get pos2 dim)) (-> r
                                                      (update-in [idx-1 :velocity dim] inc)
                                                      (update-in [idx-2 :velocity dim] dec))
                 :else r))))))

    moons
    (generate-pairs (count moons))))

(defn energy [v]
  (->> v
       (map math/abs)
       (reduce +)))

(defn step-all [particles]
  (->> particles
       (gravity 0)
       (gravity 1)
       (gravity 2)
       (mapv
         (fn [particle]
           (-> particle
               (update-in [:position] #(mapv + % (:velocity particle))))))))

(defn step-single [dim particles]
  (->> (gravity dim particles)
       (mapv
         (fn [particle]
           (-> particle
               (update-in [:position] #(mapv + % (:velocity particle))))))))

(defn first-duplicate [coll]
  (loop [s #{}
         coll coll
         i 0]
    (if (contains? s (first coll))
        i
        (recur (conj s (first coll)) (drop 1 coll) (inc i)))))

(defn lcm [& xs]
  (let [x (apply max xs)]
    (loop [n 2]
      (let [r (* n x)]
        (if (every? #(zero? (mod r %)) xs)
          r
          (recur (inc n)))))))

(defn part-1 []
  (->> (nth (iterate step-all input) 1000)
       (map (fn [moon]
              (let [{:keys [position velocity]} moon]
                (* (energy position) (energy velocity)))))
       (reduce +)))


(defn part-2 []
  ;; I did the LCM using an online tool; @TODO write LCM function for >2 numbers
  (math/lcm
    (math/lcm
      (first-duplicate (iterate (partial step-single 0) input))
      (first-duplicate (iterate (partial step-single 1) input)))
    (first-duplicate (iterate (partial step-single 2) input))))
