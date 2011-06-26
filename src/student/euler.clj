(ns student.euler
  (:import java.lang.Math))

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

(defn divisible?
  [x divisor]
  (zero? (mod x divisor)))

(defn prime?
  "Could refine this to limit the divisors to the sqrt of x"
  [x]
  (not-any? #(divisible? x %) (range 2 (dec x))))

(defn primes
  "Generates an infinite list of prime numbers. Probably highly inefficient."
  []
  (let [odd-nums (filter odd? (iterate inc 3))]
    (conj (filter prime? odd-nums) 2)))

(defn euler-prime-factor
  [num]
  ( filter #(divisible? num %) (take-while #(< % num) (primes))))

(defn palindrome?
  [string]
  (= string (apply str (reverse string))))

(defn prodcuts
  [bound]
  (flatten (for [x (range 1 (dec bound))]
             (for [y (range 1 (dec bound))]
               (* x y)))))

(defn euler-largest-palindrome
  ( []
      (euler-largest-palindrome 1000))
  ( [bound]
      (apply max (map #(Integer. %)
                      (filter palindrome? (map str (prodcuts bound)))))))

(defn euler-divisible-by-range
  ([]
     (euler-divisible-by-range (range 1 21)))
  ([range]
     (loop [x 1]
       (if (every? #(divisible? x %) range)
         x
         (recur (inc x))))))

(defn sum
  [coll]
  (apply + coll))

"Had to hack in a load of clojure.contrib.math here - should pull that out when I work out where it has gone in 1.3"
(derive ::integer ::exact)
(derive java.lang.Integer ::integer)
(derive java.math.BigInteger ::integer)
(derive java.lang.Long ::integer)
(derive java.math.BigDecimal ::exact)
(derive clojure.lang.Ratio ::exact)
(derive java.lang.Double ::inexact)
(derive java.lang.Float ::inexact)

(defmulti #^{:arglists '([base pow])
	     :doc "(expt base pow) is base to the pow power.
Returns an exact number if the base is an exact number and the power is an integer, otherwise returns a double."}
  expt (fn [x y] [(class x) (class y)]))

(defn- expt-int [base pow]
  (loop [n pow, y (num 1), z base]
    (let [t (bit-and n 1), n (bit-shift-right n 1)]
      (cond
       (zero? t) (recur n y (* z z))
       (zero? n) (* z y)
       :else (recur n (* z y) (* z z))))))

(defmethod expt [::exact ::integer] [base pow]
  (cond
   (pos? pow) (expt-int base pow)
   (zero? pow) 1
   :else (/ 1 (expt-int base (- pow)))))

(defmethod expt :default [base pow] (Math/pow base pow))

(defn euler-sum-of-sqares
  " Problem 6. Determines the difference between the sum of the squares of a range, and the square of the sum of the range" 
  ([]
     (euler-sum-of-sqares (range 1 101)))
  ([range]
     (- (expt (sum range) 2) (sum (map #(expt % 2) range)))))
