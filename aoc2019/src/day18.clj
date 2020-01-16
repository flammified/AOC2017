(ns day18
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.grids :as grids]
            [clojure.math.combinatorics :refer [cartesian-product]]))


(def input
  (-> "day18/input.txt"
      io/resource io/file slurp
      (str/split-lines)
      (->> (mapv #(vec (char-array %))))))

(def input-part-2
  (-> "day18/input-part-2.txt"
      io/resource io/file slurp
      (str/split-lines)
      (->> (mapv #(vec (char-array %))))))


(defn dependency-graph [grid start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0 []])
         visited (set nil)
         reachable-keys []]
    (if (empty? queue)
      reachable-keys
      (let [[current distance needed-keys] (peek queue)
            tile (get grid current)]
        (recur
          (reduce
            (fn [queue direction]
              (let [next (grids/step current direction)
                    tile (get grid next)]
                (if (and (some? tile) (not (contains? visited next)))
                  (cond
                    (= \# tile) queue
                    (Character/isUpperCase tile)
                    (conj queue [next (inc distance) (conj needed-keys (Character/toLowerCase tile))])
                    :else (conj queue [next (inc distance) needed-keys]))
                  queue)))
            (pop queue)
            [:north :east :south :west])
          (conj visited current)
          (if (Character/isLowerCase tile)
              (conj reachable-keys [current tile distance needed-keys])
              reachable-keys))))))

(def reachable-keys (memoize reachable-keys))

(def shortest-path
  (memoize
    (fn [graph start collected-keys]
      (let [optional-keys (get graph start)
            keys-to-get (filter (fn [[coord key distance dependencies]]
                                  (not (contains? collected-keys key)))
                                optional-keys)]
        (if (empty? keys-to-get)
          0
          (->> keys-to-get
               (filter (fn [[coord key distance dependencies]]
                        (every?
                          (fn [key]
                            (contains? collected-keys key))
                          dependencies)))
               (map (fn [[coord key distance dependencies]]
                      (+ distance
                         (shortest-path
                           graph
                           key
                           (set (conj collected-keys key))))))
               (apply min)))))))

(def shortest-paths
  (memoize
    (fn [graph starts collected-keys]
      (let [keys-to-get (->> starts
                            (map #(get graph %))
                            (map #(filter (fn [[coord key distance dependencies]]
                                            (not (contains? collected-keys key)))
                                          %))
                            (map #(filter (fn [[coord key distance dependencies]]
                                           (every?
                                             (fn [key]
                                               (contains? collected-keys key))
                                             dependencies))
                                          %))
                            (map-indexed (fn [idx keys] (map #(conj % idx) keys)))
                            (apply concat))]
        (if (empty? keys-to-get)
          0
          (->> keys-to-get
               (map (fn [[coord key distance dependencies robot]]
                      (+ distance (shortest-paths graph (assoc starts robot key) (set (conj collected-keys key))))))
               (apply min)))))))




(defn char-array-to-map [ca]
  (into {}
    (for [y (range (count ca))
          x (range (count (get ca 0)))]
      [[x y] (get-in ca [y x])])))

(defn find-in-map [hm val]
  (first
    (filter
      #(= val (get hm %))
      (keys hm))))

(def letters (conj (map char (range 97 123)) \@ \% \^ \&))

(defn part-1 []
  (let [grid (char-array-to-map input)
        graph (->> letters
                   (map (fn [l]
                          (if-let [coord (find-in-map grid l)]
                            [l (dependency-graph grid coord)])))
                  (into {}))]
    (shortest-path graph \@ [])))

(defn part-2 []
  (let [grid (char-array-to-map input-part-2)
        graph (->> letters
                   (map (fn [l]
                          (if-let [coord (find-in-map grid l)]
                            [l (dependency-graph grid coord)])))
                   (filter some?)
                   (into {}))]
    (shortest-paths graph [\@ \% \^ \&] [])))
