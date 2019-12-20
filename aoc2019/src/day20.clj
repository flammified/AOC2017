(ns day20
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [util.intcode :as intcode :refer [run-sync]]
            [clojure.core.async :as async :refer [poll! >!! <!! to-chan chan offer!]]
            [util.grids :as grids :refer [draw-sparse]]))


(def input
  (-> "day20/input.txt"
      io/resource io/file slurp
      (str/split-lines)
      (->> (mapv #(vec (char-array %))))))

(defn portal-at [grid [w h] [x y]]
  (reduce
    (fn [_ dir]
      (let [current (get-in grid [y x])
            [nx ny] (grids/step [x y] dir)
            tile (get-in grid [ny nx])
            [tx ty] (grids/step [nx ny] dir)]
        (if (= current \.)
          (if (and (some? tile) (Character/isUpperCase tile))
            (reduced {:label (->> [[nx ny] [tx ty]]
                                  (sort-by (juxt first second))
                                  (map #(get-in grid (reverse %))))
                      :up (and (> x 5) (< x (- w 5)) (> y 5) (< y (- h 5)))})
            nil))))
    false
    [:north :east :south :west]))


(defn parse [i]
  (let [h (count i)
        w (count (get i 2))]
    (into {}
      (for [x (range w)
            y (range h)]
        (let [tile (get-in i [y x])]
          [[x y] {:tile tile
                  :portal (portal-at i [w h] [x y])}])))))


(defn find-in-map [grid k]
  (reduce
    (fn [res t]
      (if (= (-> (get grid t) :portal :label) k)
        (conj res t)
        res))
    []
    (keys grid)))

(defn go-through-portal [grid position]
    (if-let [key (-> (get grid position) :portal :label)]
      (let [others (find-in-map grid key)
            [f s] others]
        (if (= f position)
          s
          f))))

(defn go-through-portal-2 [grid [x y] layer]
  (if-let [key (get-in grid [[x y] :portal :label])]
    (let [others (find-in-map grid key)
          up? (get-in grid [[x y] :portal :up])
          f (case (get-in grid [[x y] :portal :up]) true inc dec)
          [[fx fy] [sx sy]] others]
      (if (and (not up?) (= layer 0))
        nil
        (do
          ; (println "KEY" key "LAYER" (f layer) "F" f)
          (if (and (> layer 0) (or (= key [\A \A]) (= key [\Z \Z])))
            nil
            (if (and (> (count others) 1) (= [fx fy] [x y]))
              [[sx sy] (f layer)]
              [[fx fy] (f layer)])))))))

(defn bfs [grid start end]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0])
         visited {start true}
         result {}]
    (if (empty? queue)
      result
      (let [[current distance] (peek queue)]
        (recur
          (->> (reduce
                 (fn [new-queue dir]
                   (let [next (grids/step current dir)
                         {:keys [tile]} (get grid next)]
                     (if (get visited next)
                       new-queue
                       (case tile
                         \# new-queue
                         \space new-queue
                         nil new-queue
                         \. (conj new-queue [next (inc distance)])
                         new-queue))))
                 (pop queue)
                 [:north :east :south :west])
            ((fn [queue]
               (let [other (go-through-portal grid current)]
                 (if (and (some? other) (nil? (get visited other)))
                   (conj queue [other (inc distance)])
                   queue)))))
          (assoc visited current true)
          (assoc result current distance))))))

(defn bfs-recursive [grid start end]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start 0 0])
         visited {start true}
         result {}]
    (if (empty? queue)
      result
      (let [[current distance layer] (peek queue)]
        ; (println current distance layer)
        (spit "layers.txt" (str layer "\n") :append true)
        (spit "distance.txt" (str distance "\n") :append true)
        (if (and (= current end) (= layer 0))
          distance
          (recur
            (->> (reduce
                   (fn [new-queue dir]
                     (let [next (grids/step current dir)
                           {:keys [tile]} (get grid next)]
                       (if (get visited (conj next layer))
                         new-queue
                         (case tile
                           \# new-queue
                           \space new-queue
                           nil new-queue
                           \. (conj new-queue [next (inc distance) layer])
                           new-queue))))
                   (pop queue)
                   [:north :east :south :west])
                 ((fn [queue]
                     (let [[other layer] (go-through-portal-2 grid current layer)]
                       (if (and (some? other) (nil? (get visited (conj other layer))))
                         (conj queue [other (inc distance) layer])
                         queue)))))
            (assoc visited (conj current layer) true)
            (assoc result current distance)))))))

(defn part-1 []
  (let [grid (parse input)
        start (first (find-in-map grid [\A \A]))
        end (first (find-in-map grid [\Z \Z]))]
    (get (bfs grid start end) end)))


(defn part-2 []
  (let [grid (parse input)
        start (first (find-in-map grid [\A \A]))
        end (first (find-in-map grid [\Z \Z]))]
    (println (find-in-map grid [\A \A]))
    (bfs-recursive grid start end)))


(println (part-2))
