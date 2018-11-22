(ns day15)

(defn generator [value factor modulo]
  (let [next-value (mod (* value factor) 2147483647)]
    (if (== (mod next-value modulo) 0)
      (cons next-value (lazy-seq (generator next-value factor modulo)))
      (lazy-seq (generator next-value factor modulo)))))

(defn pair? [[value_a value_b]]
  (let [a_16_bits (bit-and value_a 0xffff)
        b_16_bits (bit-and value_b 0xffff)]
    (== a_16_bits b_16_bits)))

(defn part-1 [])

(defn part-2 []
  (let [generator_a (take 5000000 (generator 512 16807 4))
        generator_b (take 5000000 (generator 191 48271 8))]

    (count (filter pair? (map vector generator_a generator_b)))))
