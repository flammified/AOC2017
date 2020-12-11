(ns utils.collections)


(defn first-duplicate [coll]
  (let [visited #{}]
    (reduce
      (fn [s a]
        (if (contains? s a)
          (reduced a)
          (conj s a)))
      visited
      coll)))
