(ns day20
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]))

(def input
  (-> "day20/input.txt" io/resource io/file slurp))

(def particles (->> input
                    (str/split-lines)
                    (mapv #(re-seq #"-?\d+" %))
                    (mapv #(mapv read-string %))
                    (mapv #(partition 3 %))
                    (map-indexed (fn [idx particle]
                                   {:index idx :pos (nth particle 0) :vel (nth particle 1) :acc (nth particle 2)}))))


(defn update-particle [particle]
  (let [new-velocity (map + (:vel particle) (:acc particle))
        new-position (map + (:pos particle) new-velocity)]
    (-> particle
         (assoc :vel new-velocity)
         (assoc :pos new-position))))

(defn remove-collisions [particles]
  (let [freqs (frequencies (map :pos particles))]
    (filter #(= (get freqs (:pos %)) 1) particles)))

(defn step-without-collision [particles]
  (map update-particle particles))

(defn step-with-collision [particles]
  (->> particles
      (map update-particle)
      remove-collisions))


(defn distance [v]
  ; (println v))
  (+ (math/abs (nth v 0)) (math/abs (nth v 1)) (math/abs (nth v 2))))



(defn part-1 []
  (first (sort-by #(distance (:pos %)) (nth (iterate step-without-collision particles) 500))))

(defn part-2 []
  (count (last (take 10000 (iterate step-with-collision particles)))))
