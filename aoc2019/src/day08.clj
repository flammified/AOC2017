(ns day08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn])

  (:use [loom.graph]
        [loom.alg]))

(def input
  (-> "day08/input.txt"
      io/resource io/file slurp str/trim (str/split #"")
      (->> (mapv edn/read-string) (partition 25) (partition 6))))



(defn get-xyz [input x y z]
  (-> input (nth z) (nth x) (nth y)))

(defn set-in-stack [input d w l]
  (reduce
    #(assoc %1 %2 (apply (partial get-xyz input) %2))
    {}
    (for [z (range d)
          x (range w)
          y (range l)]
      [x y z])))

(defn render-pixel [stack x y d]
  (first (drop-while
           #(= 2 %)
           (map #(get stack %)
             (for [z (range d)]
              [x y z])))))


(defn render [input]
  (let [depth (count input)
        width (count (first input))
        length (count (first (first input)))]

    (let [stack (set-in-stack input depth width length)]
      (str/join
        "\n"
        (map
          (fn [x]
            (apply str
              (map
                (fn [y]
                  (render-pixel stack x y depth))
                (range length))))
          (range width))))))

(defn part-1 []
  (let [input (map #(apply concat %) input)
        layer (first (sort-by #(count (filter #{0} %)) input))]
    (* (count (filter #{1} layer)) (count (filter #{2} layer)))))

(defn part-2 []
  (-> (render input) (str/replace  #"0" " ") (str/replace #"1" "█")))
