(ns day04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [clojure.edn :as edn]))

;; THIS NEEDS CLEANUP
;; Holy shit

(defn parse-line [line]
  (let [[year month day hour minute guard] (->> line (re-seq #"\d+")
                                                (map #(Integer/parseInt %))
                                                (into []))
        logtype (cond
                  (str/includes? line "asleep") :asleep
                  (str/includes? line "wakes") :awake
                  (str/includes? line "begins") :start
                  :else :invalid)
        log [year month day hour minute logtype guard]]
    (zipmap [:year :month :day :hour :minute :logtype :guard] log)))


(def text
  (-> "day04/input.txt" io/resource io/file slurp))

(def input (->> text
                str/split-lines
                (map parse-line)))

(defn add-minutes [guard from to]
  (reduce #(update %1 %2 (fnil inc 0)) guard (range from to)))

(defn accumulate-logs [logs]
  (reduce
    (fn [{:keys [current-guard last-sleep] :as state} {:keys [day hour minute logtype guard] :as log}]
      (case logtype
        :start (-> state
                   (assoc :current-guard guard)
                   (cond-> (nil? (get-in state [:guards guard])) (assoc-in [:guards guard] {:total 0 :minutes {}})))
        :asleep (assoc state :last-sleep minute)
        :awake (-> state
                   (update-in [:guards current-guard :total] + (- minute last-sleep))
                   (update-in [:guards current-guard :minutes] add-minutes last-sleep minute))))
    {}
    logs))

(defn strategy-1 [logs]
  (let [current-state (accumulate-logs (sort-by (juxt :year :month :day :hour :minute) input))
        [guard {:keys [minutes]}] (->> current-state
                                       :guards
                                       (sort-by #(get-in (val %) [:total]))
                                       last)
        max-minute (first (apply max-key val minutes))]
    [max-minute guard]))

(defn strategy-2 [logs]
  (let [current-state (accumulate-logs (sort-by (juxt :year :month :day :hour :minute) input))]
    (->> current-state
         :guards
         seq
         (map (fn [[guard info]] [guard (->> info :minutes (sort-by val >) first)]))
         (filter #(some? (second (second %))))
         (sort-by #(second (second %)) >)
         first)))

(defn part-1 []
  (apply * (strategy-1 (sort-by (juxt :year :month :day :hour :minute) input))))

(defn part-2 []
  (let [[guard [minute amount]] (strategy-2 (sort-by (juxt :year :month :day :hour :minute) input))]
    (* guard minute)))
