(ns day13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def text
  (-> "day13/input.txt" io/resource io/file slurp))


(defn parse-line [state line]
  (let [ [id-str length-str] (str/split line #": ")
         id (read-string id-str)
         length (read-string length-str)]

    (assoc state id {:length length})))

(defn create-initial-state [text]
  (let [firewall (reduce parse-line {} (str/split-lines text))]
    {:firewall firewall :cycle 0}))

(defn position-at-cycle [cycle length]
  (if (even? (quot cycle (- length 1)))
    (mod cycle (- length 1))
    (- length (mod cycle (- length 1)))))

(defn increase-cycle [state]
  (update-in state [:cycle] inc))

(defn calculate-current-severity [state]
  (let [current-position (:cycle state)
        firewall-length (:length (get (:firewall state) current-position))]
    (if (nil? firewall-length)
      0
      (if (== (position-at-cycle (:cycle state) firewall-length) 0)
        (* firewall-length current-position)
        0))))

(defn calculate-severity-of-trip [state from to]
  (if (> from to)
    0
    (+ (calculate-current-severity state) (calculate-severity-of-trip (increase-cycle state) (inc from) to))))

(defn caught? [state delay]
  (let [current-position (:cycle state)
        firewall-length (:length (get (:firewall state) current-position))]
    (if (nil? firewall-length)
      false
      (if (== (position-at-cycle (+ delay (:cycle state)) firewall-length) 0)
        true
        false))))

(defn caught-in-trip [state from to delay]
  (if (> from to)
    false
    (if (caught? state delay)
      true
      (recur (increase-cycle state) (inc from) to delay))))

(defn max-index [state]
  (apply max (keys (:firewall state))))

(defn smallest-delay [state]
  (first (filter #(not (caught-in-trip state 0 (max-index state) %1)) (range 0 100000000))))

(defn part-1 []
  (let [initial (create-initial-state text)]
    (calculate-severity-of-trip initial 0 92)))

(defn part-2 []
  (let [initial (create-initial-state text)]
    (smallest-delay initial)))
