(ns day21
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.core.matrix :as matrix]))

(defn matrix-to-string [matrix]
  (str/join "/" (map #(apply str %) matrix)))

(defn string-to-matrix [matrix-string]
  (map #(str/split % #"") (str/split matrix-string #"/")))






(def start-matrix
  [[\. \# \.]
   [\. \. \#]
   [\# \# \#]])


(defn flip-matrix [m]
  (map reverse m))

(defn rotate [m n]
  (if (<= n 0)
    m
    (recur (map reverse
             (apply map vector m))
           (dec n))))

(defn possible-forms-of [m]
  (let [matrix-vector (string-to-matrix m)]
    (map
      matrix-to-string
      [matrix-vector
       (rotate matrix-vector 1)
       (rotate matrix-vector 2)
       (rotate matrix-vector 3)
       (flip-matrix matrix-vector)
       (flip-matrix (rotate matrix-vector 1))
       (flip-matrix (rotate matrix-vector 2))
       (flip-matrix (rotate matrix-vector 3))])))

(def input
  (-> "day21/input.txt" io/resource io/file slurp str/trim-newline))

(def rulebook
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (mapcat (fn [[from _ to]] (zipmap (possible-forms-of from) (repeat to))))
       (into {})))


(defn split-matrix-into-parts [mat]
    (let [[width height] (matrix/shape mat)
          size (cond
                       (== (mod width 2) 0 ) 2
                       (== (mod width 3) 0 ) 3
                       :else 2)
          amount-of-pieces (/ width size)]
      (map
          (fn [row-index]
              (map
                (fn [col-index]
                  (matrix/submatrix
                    mat
                    (* size row-index)
                    size
                    (* size col-index)
                    size))
                (range amount-of-pieces)))
       (range amount-of-pieces))))

(def apply-rulebook
    (memoize
        (fn [rulebook m]
            (if (nil? (get rulebook (matrix-to-string m)))
              m
              (string-to-matrix (get rulebook (matrix-to-string m)))))))

(defn join-matrix-parts [parts]
  (reduce
      (fn [new-matrix matrix-parts-row]
        (concat new-matrix (apply map (fn [& colls] (apply concat colls)) matrix-parts-row)))
      []
      parts))


(def grow
  (memoize
    (fn [mat rules]
        (->> mat
          split-matrix-into-parts
          (map (fn [row] (map #(apply-rulebook rules %) row)))
          join-matrix-parts))))

(defn iterate-matrix [iterations current-matrix rules]
  (loop [iterations iterations
         current-matrix current-matrix]
    (println iterations)
    (if (<= iterations 0)
      current-matrix
      (recur (dec iterations) (grow current-matrix rules)))))

(defn count-on [m]
  (->> m
    flatten
    (filter #(= "#" %))
    count))

(defn part-1 []
  (count-on (iterate-matrix 5 start-matrix rulebook)))

(defn part-2 []
  (count-on (iterate-matrix 18 start-matrix rulebook)))

; (defn -main [args]
;   (println (iterate-matrix 5 start-matrix)))
