(set-env!
 :dependencies '[[org.clojure/clojure "1.9.0"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [net.mikera/core.matrix "0.62.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/core.async "0.6.532"]
                 [aysylu/loom "1.0.2"]
                 [com.taoensso/tufte "2.1.0"]


                 ;; Visualization stuff
                 [quil/quil "2.2.4"]
                 [gil "1.0.0-SNAPSHOT"]]

 :resource-paths #{"resources" "src"})

(task-options!
 repl {:eval '(set! *print-length* 20)})  (println "Dev profile running")

(deftask dev
  "Profile setup for development.
  	Starting the repl with the dev profile...
  	boot dev repl "
  []
  (set-env!
   :init-ns 'user
   :source-paths #(into % ["src"])
   :dependencies #(into % '[[org.clojure/tools.namespace "0.2.11"] [proto-repl "0.3.1"]]))

  ;; Makes clojure.tools.namespace.repl work per https://github.com/boot-clj/boot/wiki/Repl-reloading
  (require 'clojure.tools.namespace.repl)
  (eval '(apply clojure.tools.namespace.repl/set-refresh-dirs
                (get-env :directories)))

  identity)

(defn run-day-part
  [day part]
  (try
    (let [day (format "day%02d" day)
          sym (symbol day
                      (str "part-" part))
          _   (require (symbol day))
          fn  (resolve sym)]
      (if (not (nil? fn))
        (println (str sym " |") (time (fn)))))
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
  (println "---------------------------------------")
  (println "|              AoC 2019               |")
  (println "---------------------------------------")
  (with-pass-thru [_]
    (doseq [day (range 1 (inc 25))
            part [1 2]]
      (run-day-part day part))))
