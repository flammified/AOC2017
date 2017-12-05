(ns day4-1.core
  (require [clojure.string :as str]))

(def input (slurp "/tmp/input.txt"))

(defn sort-word [word]
  (str (sort (seq (char-array word))))


  (defn valid-2? [passphrase]
    (== 0 (count
            (filter #(> (second %) 1)
              (reduce
                (fn [m k] (update-in m [(sort-word (str/lower-case k)) (fnil inc 0)]))
                {}
                (str/split passphrase #" ")))))))

(defn valid-1? [passphrase]
  (== 0 (count
          (filter #(> (second %) 1)
            (reduce
              (fn [m k] (update-in m [(str/lower-case k)] (fnil inc 0)))
              {}
              (str/split passphrase #" "))))))


(defn -main []
  (println (count
            (filter
              valid-2?
              (str/split-lines input)))))
