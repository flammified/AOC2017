(ns day18
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def text
  (-> "day18/input.txt" io/resource io/file slurp))
