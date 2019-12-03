(ns day03
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.math.numeric-tower :as math]))


(defn parse-delta [line]
  (let [dir (str (first line))
        length (edn/read-string (apply str (rest line)))]
    {:direction dir :length length}))

(defn apply-direction [[x y] dir length]
  (case dir
    "R" [(+ x length) y]
    "U" [x (+ y length)]
    "L" [(- x length) y]
    "D" [x (- y length)]))

(defn generate-steps [position direction length]
  (for [step (range (inc length))]
    (apply-direction position direction step)))

(defn add-steps-to-grid [grid steps total-steps]
  (reduce-kv
      (fn [grid idx step]
        (let [step-state (get grid step)]
          (if (not (:visited step-state))
            (assoc-in grid [step] {:visited true :steps (+ total-steps idx)})
            grid)))
      grid
      (vec steps)))

(defn create-grid [position deltas]
  (reduce
    (fn [{:keys [position grid total-steps] :as state} {:keys [direction length]}]
      (let [steps (generate-steps position direction length)]
        (-> state
            (assoc :grid (add-steps-to-grid grid steps total-steps))
            (assoc :position (last steps))
            (assoc :total-steps (+ length total-steps)))))
    {:position position :grid {} :total-steps 0}
    deltas))

(defn find-intersections [grid1 grid2]
  (->> grid1
       (keys)
       (filter #(not (= [0 0] %)))
       (filter #(some? (get grid2 %)))))

(defn manhattan [[x y]]
  (+ (math/abs x) (math/abs y)))

(defn find-closest-intersection [[line1 line2]]
  (let [grid1 (:grid (create-grid [0 0] line1))
        grid2 (:grid (create-grid [0 0] line2))
        intersections (find-intersections grid1 grid2)]
    (->> intersections
         (map manhattan)
         (filter pos?)
         (reduce min))))

(defn find-intersection-with-least-steps [[line1 line2]]
  (let [grid1 (:grid (create-grid [0 0] line1))
        grid2 (:grid (create-grid [0 0] line2))
        intersections (find-intersections grid1 grid2)]
    (->> intersections
         (reduce #(conj %1 [%2 (+ (get-in grid1 [%2 :steps]) (get-in grid2 [%2 :steps]))]) [])
         (apply min-key second)
         (second))))


(def input
  (-> "day03/input.txt"
      io/resource io/file slurp str/trim
      (str/split-lines)
      (->> (map (fn [line] (str/split line #",")))
           (map (fn [line] (map parse-delta line))))))



(defn part-1 []
  (find-closest-intersection input))


(defn part-2 []
  (find-intersection-with-least-steps input))
