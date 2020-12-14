(ns day13s
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [util.grids :as grids]
            [clojure.math.numeric-tower :as math]))

(defn extended-gcd
 "The extended Euclidean algorithm
 Returns a list containing the GCD and the Bézout coefficients
 corresponding to the inputs. "
 [a b]
 (cond (zero? a) [(math/abs b) 0 1]
       (zero? b) [(math/abs a) 1 0]
       :else (loop [s 0
                    s0 1
                    t 1
                    t0 0
                    r (math/abs b)
                    r0 (math/abs a)]
               (if (zero? r)
                 [r0 s0 t0]
                 (let [q (quot r0 r)]
                   (recur (- s0 (* q s)) s
                          (- t0 (* q t)) t
                          (- r0 (* q r)) r))))))

(defn chinese_remainder
 " Main routine to return the chinese remainder "
 [n a]
 (let [prod (apply * n)
       reducer (fn [sum [n_i a_i]]
                 (let [p (quot prod n_i)           ; p = prod / n_i
                       egcd (extended-gcd p n_i)   ; Extended gcd
                       inv_p (second egcd)]        ; Second item is the inverse
                   (+ sum (* a_i inv_p p))))
       sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
                                                     ; (map vector n a) is same as
       ;                                             ; Python's version Zip (n, a)
   (mod sum-prod prod)))                             ; Result line

(defn parse [[start schedule]]
  (let [start (read-string start)
        vals (str/split schedule #",")]
    {:start start
     :schedule (reduce-kv
                 (fn [s i id]
                   (case id
                     "x" s
                     (conj s {:departs (read-string id) :offset (- (dec (count vals)) i)})))
                 []
                 vals)}))


(def input
  (-> "day13/input.txt"
      io/resource
      io/file slurp
      str/trim
      str/split-lines
      parse))

(defn part-1 []
  (let [{:keys [start schedule]} input
        {:keys [id departs]} (apply min-key (fn [bus] (Math/ceil (- start (mod start (:departs bus))))) schedule)]
      (* departs (- (* departs (int (Math/ceil (/ start departs)))) start))))


(defn brute-force [schedule pin]) 



(defn part-2-lock []
  (let [{:keys [start schedule]} input]
    (reduce
      (fn))))

(defn part-2 []
  (let [{:keys [start schedule]} input
        max-id (:offset (last (sort-by :offset schedule)))
        offsets (map :offset schedule)
        departures (map :departs schedule)]
    (- (chinese_remainder departures offsets) max-id)))

(part-2-lock)

; (part-2)
