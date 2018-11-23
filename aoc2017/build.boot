(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [net.mikera/core.matrix "0.62.0"]]
 :resource-paths #{"resources" "src"})

(task-options!
 repl {:eval '(set! *print-length* 20)})

(defn run-day-part
  [day part]
  (try
    (let [day (format "day%02d" day)
          sym (symbol day
                      (str "part-" part))
          _   (require (symbol day))
          fn  (resolve sym)]
      (if (not (nil? fn))
        (println (str sym ":") (fn))))
    (catch java.io.FileNotFoundException e
      println (str (.getMessage e)))))

(deftask run-day
  [d day  VAL int "day"
   p part VAL int "part"]
  (with-pass-thru [_]
    (assert day)
    (let [parts (if part
                  [part]
                  [1 2])]
      (doseq [p parts]
        (run-day-part day p)))))

(deftask run-all
  []
  (with-pass-thru [_]
    (doseq [day (range 1 (inc 25))
            part [1 2]]
      (run-day-part day part))))
