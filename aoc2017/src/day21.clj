(ns day21_new.core
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]))

(def input (slurp "input.txt"))

(def rulebook
  (reduce
    (fn [result line]
      (let [[from _ to] (str/split line #" ")]
        (assoc result from to)))
    {}
    (str/split-lines input)))

(defn matrix-to-string [matrix]
  (str/join "/" (map #(apply str %) matrix)))

(defn string-to-matrix [matrix-string]
  (map #(str/split % #"") (str/split matrix-string #"/")))
;
(def start-matrix
  [[\. \# \.]
   [\. \. \#]
   [\# \# \#]])

; (matrix/pm start-matrix)

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
   (flip-matrix m)
   (flip-matrix (rotate m 1))
   (flip-matrix (rotate m 2))
   (flip-matrix (rotate m 3))])


(def apply-rulebook
    (memoize
        (fn [rulebook m]
          (let [possible-forms (possible-forms-of m)
                new-matrix (some
                             #(get rulebook (matrix-to-string %))
                             possible-forms)]
            (if (nil? new-matrix)
              m
              (string-to-matrix new-matrix))))))

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
    (if (<= iterations 0)
      current-matrix
      (recur (dec iterations) (grow current-matrix rules)))))


(println (map matrix-to-string (possible-forms-of start-matrix)))

(matrix/pm (iterate-matrix 5 start-matrix rules))
(println "done")

(defn count-on [m]
  (println (->> m
             flatten
             (filter #(= "#" %))
             count)))


(println (count-on (iterate-matrix 18 start-matrix rulebook)))

; (defn -main [args]
;   (println (iterate-matrix 5 start-matrix)))
