(ns student.euler)

(defn divides?
  [dividend divisor]
  (if (= (rem dividend divisor) 0) true false))

(defn divides-any?
  [divisor dividends]
  (not (nil? (some #(divides? % 5) dividends))))
