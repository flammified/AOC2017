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
  (if (nil? name)
    nil
    (get stack name)))

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
    (if (== (count subtowers) 0)
      (:weight tower)
      (+ (:weight tower)
         (reduce
           +
           (map
             (partial sum-tower stack)
             (map
               (partial child-from-name stack)
               subtowers)))))))

(defn sums-of-subtowers [stack subtowers]
  (map
    (partial sum-tower stack)
    (map
       (partial child-from-name stack)
       subtowers)))

(defn same? [vec]
  (apply = vec))

(defn odd-one-out [stack subtowers]
  (let [sums (sums-of-subtowers stack subtowers)
        a (println sums)
        odd-sum (first
                  (first
                    (filter
                      #(== 1 (second %))
                      (frequencies sums))))]
    (if (nil? odd-sum)
      nil
      (first
        (first
          (filter
            #(== odd-sum (second %))
            (map vector subtowers sums)))))))

(defn has-no-items? [sequence]
  (== (count sequence) 0))

(defn find-lowest-problem-parent [stack tower]
  (let [subtowers (:subtowers tower)]
    (if (has-no-items? subtowers)
      nil
      (let [different-value (child-from-name stack (odd-one-out stack subtowers))]
        (if (nil? different-value)
          nil
          (let [problem-in-child (find-lowest-problem-parent stack different-value)]
            (if (nil? problem-in-child)
              [tower different-value]
              problem-in-child)))))))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn random-except [haystack needle]
  (get haystack (get (vec-remove (vec (range (count haystack))) (.indexOf haystack needle)) 0)))

(defn what-to-do [stack [parent child]]
  (println parent child)
  (let [subtowers (:subtowers parent)
        name-of-child (:name child)
        correct-sum (sum-tower stack (child-from-name stack (random-except subtowers name-of-child)))
        incorrect-sum (sum-tower stack child)
        difference (- incorrect-sum correct-sum)]
    (- (:weight child) difference)))

(defn -main []
  (let [stack (create-stack text)
        random-key "ttrgg"
        random-child (get stack random-key)]
    (println "Weight should be"(what-to-do stack (find-lowest-problem-parent stack (root-tower stack random-child))))))
