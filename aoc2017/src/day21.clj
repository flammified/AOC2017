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

(def transpose (partial apply map vector))

(def flip (partial map reverse))

(def rotate-right (comp flip transpose))

(defn possible-forms-of [m]
  (let [matrix-vector (string-to-matrix m)]
    (map
      matrix-to-string
      [matrix-vector
       (nth (iterate rotate-right matrix-vector) 1)
       (nth (iterate rotate-right matrix-vector) 2)
       (nth (iterate rotate-right matrix-vector) 3)
       (flip matrix-vector)
       (flip (nth (iterate rotate-right matrix-vector) 1))
       (flip (nth (iterate rotate-right matrix-vector) 2))
       (flip (nth (iterate rotate-right matrix-vector) 3))])))

(def input
  (-> "day21/input.txt" io/resource io/file slurp str/trim-newline))

(def rulebook
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (mapcat (fn [[from _ to]] (zipmap (possible-forms-of from) (repeat to))))
       (into {})))


(defn split-grid [n grid]
  (map #(map transpose (partition n (transpose %)))
       (partition n grid)))

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
          (split-grid (if (= (mod (count mat) 2) 0) 2 3))
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
