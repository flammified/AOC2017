(ns day23
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-sync run-state]]
            [util.grids :as grids :refer [draw-sparse]]))


(def input
  (-> "day23/input.txt"
      io/resource io/file slurp str/trim
      (intcode/parse-program)))

(defn make-pcs [l]
  (reduce
    (fn [pcs id]
      (assoc pcs id (run-sync input [id])))
    {}
    (range l)))

(defn run-network []
  (let [pcs (make-pcs 50)]
    (do
        (loop [pcs pcs
               ids (cycle (range 50))
               counter 0
               NAT nil
               last-send nil]
            (let [{:keys [output position input] :as state} (get pcs (first ids))]
              (do
                (if (not (empty? output))
                  (let [[addr x y] (subvec output 0 3)]
                    (do
                      (recur (-> pcs
                                 (assoc (first ids) (-> state
                                                        (assoc :idle false)
                                                        (update :output #(vec (drop 3 %)))))
                                 (update-in [addr :input] conj x y))
                             (drop 1 ids)
                             0
                             (if (= addr 255) [x y] NAT)
                             last-send)))
                  (if (> counter 1000)
                    (do
                      (if (= NAT last-send)
                        (second last-send)
                        (recur (-> pcs
                                   (assoc (first ids) (assoc state :idle false))
                                   (update-in [0 :input] conj (first NAT) (second NAT)))
                               (drop 1 ids)
                               0
                               NAT
                               NAT)))
                    (recur (-> pcs
                               (assoc (first ids) (run-state (assoc state :idle false))))
                           (drop 1 ids)
                           (inc counter)
                           NAT
                           last-send)))))))))



(defn part-1 []
  (run-network))

(defn part-2 []
  (run-network))
