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

(defn euler-8-consecutive-numbers
  ([]
     (euler-8-consecutive-numbers "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"))
  ([number-as-string]
     (let [digits (map #(Integer/parseInt (str %)) (str number-as-string))]
       (letfn [(product [[a b c d e & rest :as full-list]]
                 (if (nil? e)
                   nil
                   (lazy-seq (cons (* a b c d e) (product (drop 1 full-list)))))
                 )]
         (apply max (product digits))))))

(defn triangle-numbers
  ([]
     (triangle-numbers 0 1))
  ([previous-num x]
     (let [triangle-num (+ previous-num x)]
       (lazy-seq (cons triangle-num (triangle-numbers triangle-num (inc x)))))))

(defn divisors
  "Works out the whole number divisors of the given value.
     foreach number up to sqrt of num
      if divisible, add disvisor and add result of division (e.g. 10/2 = 5, so add 2 & 5)"
  [num]
  (remove nil? (flatten (map #(if (divisible? num %) [% (quot num %)]) (range 1 (Math/sqrt num))))))

(defn problem-12-triangle-divisors
  "Works out which triangle number has the specified number of divisors"
  ([]
     (problem-12-triangle-divisors 500))
  ([target-divisors]
     (take 1  (filter #(>= (count (divisors %)) target-divisors) (triangle-numbers)))))











