(ns day7.core
  (:use [clojure.string :as str]))

(def text (slurp "input.txt"))

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

(defn seq-contains? [coll target]
  (some #(= target %) coll))

(defn contains-child? [child to-check]
  ; (println (:subtowers (second to-check)) child)
  (seq-contains? (:subtowers (second to-check)) (:name child)))

(defn find-previous [stack to-find]
  (second (first (filter (partial contains-child? to-find) stack))))

(defn random-name-in-map [stack]
  (second (second (get stack (rand-nth (keys stack))))))

(defn root-tower [stack from]
  (let [previous (find-previous stack from)]
    (if (nil? previous)
      from
      (recur stack previous))))

(defn -main []
  (let [stack (create-stack text)
        random-key "ttrgg"
        random-child (get stack random-key)]
    (println (root-tower stack random-child))))
