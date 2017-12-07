(ns day7.core
  (:require [clojure.string :as str]
            [day7.parsing :refer [create-stack]]))

(def text (slurp "input.txt"))

(defn seq-contains? [coll target]
  (some #(= target %) coll))

(defn contains-child? [child to-check]
  (seq-contains? (:subtowers (second to-check)) (:name child)))

(defn find-previous [stack to-find]
  (second (first (filter (partial contains-child? to-find) stack))))

(defn child-from-name [stack name]
  (get stack name))

(defn random-name-in-map [stack]
  (second (second (child-from-name (rand-nth (keys stack))))))

(defn root-tower [stack from]
  (let [previous (find-previous stack from)]
    (if (nil? previous)
      from
      (recur stack previous))))

;; Part 2

(defn sum-tower [stack tower]
  (let [subtowers (:subtowers tower)]
    (println tower)
    (if (== (count subtowers) 0)
      (:weight tower)
      (reduce
        +
        (map
          (partial sum-tower stack)
          (map
            (partial child-from-name stack)
            subtowers))))))

(defn same? [vec]
  (apply = vec))

(defn all-subtowers-sum-same? [stack tower]
  (same?
         (map
           (partial sum-tower stack)
           (map
             (partial child-from-name stack)
             (:subtowers tower)))))

(defn find-problem-parent [stack tower]
  (let [subtowers (:subtowers tower)]
    (if ((== (count subtowers) 0))
      nil
      (if (not (all-subtowers-sum-same? stack tower))
        tower
        (first (filter
                 (fn [x] (not (nil? x)))
                 (map find-problem-parent
                   (map
                     (partial child-from-name stack)
                     subtowers))))))))

(defn -main []
  (let [stack (create-stack text)
        random-key "ttrgg"
        random-child (get stack random-key)]
    (println (find-problem-parent stack (root-tower stack random-child)))))
