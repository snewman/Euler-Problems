(ns student.euler)

(defn divides?
  [dividend divisor]
  (if (= (rem dividend divisor) 0) true false))

(defn divides-any
  "Return a predicate that tests whether its arg can be
   evenly divided by any of nums."
  [& nums]
  (fn [arg]
    (boolean (some #(divides? arg %) nums))))


(defn euler-1
  "Generates the list of values less than some upper bound which is divisible by 3 or 5"
  ([]
      (euler-1 1000))
  ([upper]
      (let [divisible? (divides-any 3 5)]
        (loop [sum 0
               n 1]
          (if (>= n upper) sum
              (recur (if (divisible? n) (+ sum n) sum) (inc n)))))))


(defn euler-2
  ([]
      (euler-2 1000))
  ([upper]
      (apply + (filter (divides-any 3 5) (range upper)))))

(defn euler-3
  ([]
      (euler-3 1000))
  ([upper]
     (->> (range upper) (filter (divides-any 3 5)) (apply +))))

(defn fib
  ([]
     (fib 0N 1N))
  ([x y]
     (lazy-seq (cons (+ x y) (fib y (+ x y))))))

(defn euler-fib-sum
  ([]
     (euler-fib-sum 4000000))
  ([limit]
     (apply +  (filter even? (take-while #(< % limit) (fib))))))
