(ns day7.parsing
  (:use [clojure.string :as str]))

(defn split-line-parts [line]
  (if (nil? (index-of line "->"))
    [line nil]
    (map trim (str/split line #"->"))))

(defn parse-weight [weight]
  (read-string (apply str (filter (fn [x] (Character/isDigit x)) weight))))

(defn parse-identification [identification]
  (let [[name weight] (split identification #" ")]
    {:name name
     :weight (parse-weight weight)}))

(defn parse-subtowers [subtower-str]
  (if (nil? subtower-str)
    []
    (vec (map trim (split subtower-str #",")))))

(defn parse-line [stack line]
  (let [[identification-str subtowers-str] (split-line-parts line)
        identity (parse-identification identification-str)
        subtowers (parse-subtowers subtowers-str)
        tower {:weight (:weight identity)
               :name (:name identity)
               :subtowers subtowers}]
    (assoc stack (:name tower) tower)))

(defn create-stack [input]
  (reduce parse-line {} (str/split-lines input)))
