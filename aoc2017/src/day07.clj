(ns day07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [day7-parsing :refer [create-stack]]))

(def text
  (-> "day7/input.txt" io/resource io/file slurp))

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
    (if (== (count subtowers) 0)
      (:weight tower)
      (->> subtowers
        (map (partial child-from-name stack))
        (map (partial sum-tower stack))
        (reduce +)
        (+ (:weight tower))))))


(defn sums-of-subtowers [stack subtowers]
  (->> subtowers
    (map (partial child-from-name stack))
    (map (partial sum-tower stack))))

(defn odd-one-out [stack subtowers]
  (let [sums (sums-of-subtowers stack subtowers)
        [odd-sum _] (some
                      #(if (== 1 (second %)) %)
                      (frequencies sums))]

    (if (nil? odd-sum)
      nil
      (ffirst
        (filter
          #(== odd-sum (second %))
          (map vector subtowers sums))))))

(defn has-no-items? [sequence]
  (== (count sequence) 0))

(defn find-lowest-problem-parent [stack tower]
  (let [subtowers (:subtowers tower)
        child-with-problem (child-from-name stack (odd-one-out stack subtowers))]
    (if (nil? child-with-problem)
      nil
      (let [problem-in-child (find-lowest-problem-parent stack child-with-problem)]
        (if (nil? problem-in-child)
          [tower child-with-problem]
          problem-in-child)))))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn random-except [haystack needle]
  (get haystack (get (vec-remove (vec (range (count haystack))) (.indexOf haystack needle)) 0)))

(defn what-to-do [stack [parent child]]
  (let [subtowers (:subtowers parent)
        name-of-child (:name child)
        correct-sum (sum-tower stack (child-from-name stack (random-except subtowers name-of-child)))
        incorrect-sum (sum-tower stack child)
        difference (- incorrect-sum correct-sum)]
    (- (:weight child) difference)))

(defn part-1 [])

(defn part-2 []
  (let [stack (create-stack text)
        random-key "ttrgg"
        random-child (get stack random-key)]
    (println stack)
    (println "Weight should be" (what-to-do stack (find-lowest-problem-parent stack (root-tower stack random-child))))
    (what-to-do stack (find-lowest-problem-parent stack (root-tower stack random-child)))))
