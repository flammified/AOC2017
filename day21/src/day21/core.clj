(ns day21.core
  (:require [clojure.string :as str]
        [clojure.core.matrix :as matrix]))

(def input (slurp "input.txt"))

(def rules
  (let [rulebook-map (reduce
                       (fn [result line]
                        (let [[from _ to] (str/split line #" ")]
                          (assoc result from to)))
                      {}
                      (str/split-lines input))]
    rulebook-map))

(defn matrix-to-string [matrix]
  (str/join "/" (map #(apply str %) matrix)))

(defn string-to-matrix [matrix-string]
  (map #(str/split % #"") (str/split matrix-string #"/")))
;
(def start-matrix
  [[\. \# \.]
   [\. \. \#]
   [\# \# \#]])

(matrix/pm start-matrix)

(defn matrix-coordinates [size]
  (for [y (range 0 size)
        x (range 0 size)]
    [x y]))

(matrix/pm (matrix-coordinates 2))

(defn split-matrix-into-parts-of [m size]
  (let [[width height] (matrix/shape m)
        amount-of-pieces (/ width size)
        coordinates (matrix-coordinates amount-of-pieces)]
    (matrix/pm coordinates)
    (matrix/emap
      (fn [subm]
        (matrix/submatrix
          m
          (* size (:x subm))
          size
          (* size (:y subm))
          size))
      coordinates)))

(defn flip-matrix [m]
  (map reverse m))

(defn rotate [m n]
  (if (<= n 0)
    m
    (recur (map reverse
             (apply map vector m))
           (dec n))))


(defn possible-forms-of [m]
  [m
   (rotate m 1)
   (rotate m 2)
   (rotate m 3)
   (flip-matrix m)])


(defn apply-rulebook [rulebook m]
  ; (println "matrix" m)
  (let [possible-forms (possible-forms-of m)
        ; _ (println possible-forms)
        new-value (first
                    (some
                      #(contains? rulebook (matrix-to-string %))
                      possible-forms))]
    (if (nil? new-value)
      m
      new-value)))

(defn join-matrix-parts [parts]
  (println "Input" parts))

(defn iterate-matrix [iterations current-matrix]
  (println iterations)
  (if (<= iterations 0)
    current-matrix
    (let [[width height] (matrix/shape current-matrix)
          split-size (cond
                       (== (mod width 2) 0 ) 2
                       (== (mod width 3) 0 ) 3
                       :else 2)
          _4 (println "I got here")
          parts (split-matrix-into-parts-of current-matrix split-size)
          _5 (matrix/pm parts)
          _2 (println "parts" parts)
          applied-parts (map #(apply-rulebook rules %) parts)
          new-matrix (join-matrix-parts applied-parts)
          _3 (println new-matrix)]
      (recur (dec iterations) new-matrix))))

(iterate-matrix 2 start-matrix)

; (defn -main [args]
;   (println (iterate-matrix 5 start-matrix)))
