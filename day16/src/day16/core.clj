(ns day16.core
  (:use [clojure.string :as str]))

(def text (slurp "input.txt"))

(defn parse-line [state line]
  (let [first-letter (first line)
        rest (join (rest line))]

    (println first-letter rest)
    (case first-letter
      \s (conj state (vec (conj [:spin] (read-string rest))))
      \x (conj state (vec (concat [:exchange] (map read-string (str/split rest #"\/")))))
      \p (conj state (vec (concat [:partner] (map #(first (char-array %)) (str/split rest #"\/"))))))))

(defn parse-input [input]
  (reduce parse-line [] (str/split input #",")))

(defn char-range [start end]
  (vec (map char (range (int start) (inc (int end))))))

(defn spin [input x]
  (vec (take (count input) (drop (- (count input) x) (cycle input)))))

(defn swap [items i j]
  (assoc items i (nth items j) j (nth items i)))

(defn third [input]
  (nth input 2))

(defn run-instruction [input instruction]
  (let [operation (first instruction)]
    (case operation
      :spin (spin input (second instruction))
      :exchange (swap input (second instruction) (third instruction))
      :partner (swap input (.indexOf input (second instruction)) (.indexOf input (third instruction))))))

(defn run-program [program input]
  (reduce run-instruction input program))

(defn seq-contains? [coll target] (some #(= target %) coll))

(defn run-x-times [program input x outputs]
  (println x)
  (if (<= x 0)
    input
    (recur program (run-program program input) (dec x) (conj outputs input))))

(defn -main []
  (let [parsed (parse-input text)]
    (println (join (run-x-times parsed (char-range \a \p) (mod 1000000000 60) [])))))
