(ns student.dialect
  (:require [clojure.string :as str]))

(defn canadianize
  [sentence]
  (str/replace sentence #"\.$" ", eh?"))


(defn pig-latinize
  [[ first & rest]]
  (str (apply str rest) (str first) "ay"))
