(ns viz.day3
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [day03 :refer [input apply-direction]])
  (:use gil.core))

(defn generate-steps [position direction length]
  (for [step (range (inc length))]
    (apply-direction position direction step)))

(defn create-path [position deltas]
  (reduce
    (fn [{:keys [position path] :as state} {:keys [direction length]}]
      (let [step (apply-direction position direction length)]
        {:position step :path (conj path step)}))
    {:position position :path []}
    deltas))

(def line1 (vec (concat [[0 0]] (:path (create-path [0 0] (first input))))))
(def line2 (vec (concat [[0 0]] (:path (create-path [0 0] (second input))))))

(defn setup []
  (q/frame-rate 3)
  {:counter 0})

(defn draw [state]
  (q/background 0)
  (q/stroke 255 0 0)
  (q/with-translation [600 500]
    (q/scale 0.5)
    (doseq [i (range 0 (:counter state))]
      (let [[x1 y1] (get-in line1 [i])
            [x2 y2] (get-in line1 [(inc i)])]
        (q/line (/ x1 10) (/ y1 10) (/ x2 10) (/ y2 10)))))
  (q/stroke 0 255 0)
  (q/with-translation [600 500]
    (q/scale 0.5)
    (doseq [i (range 0 (:counter state))]
      (let [[x1 y1] (get-in line2 [i])
            [x2 y2] (get-in line2 [(inc i)])]
        (q/line (/ x1 10) (/ y1 10) (/ x2 10) (/ y2 10)))))
  (save-animation "lines.gif" 300 0))

(defn update [state]
  (println (:counter state))
  (if (and (< (:counter state) (count line2)) (< (:counter state) (count line1)))
    (update-in state [:counter] #(+ 1 %))
    state))

(defn start-sketch []
  (q/defsketch lines
    :host "host"
    :size [1000 1000]
    :setup setup
    :draw draw
    :update update
    :middleware [m/fun-mode]))
