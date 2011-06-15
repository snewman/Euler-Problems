(ns student.time
  (:use '[clojure.data.json :only (read-json)])
  (:use solutions.fight))

(defn create
  ([]
     (create {}))
  ([initial-map]
     (atom initial-map)))

(defn cache-get
  [key cache]
  (@cache key))

(defn cache-put
  ([new_values cache]
     (swap! cache merge new_values))
  ([key value cache]
     (swap! cache assoc key value)))

(defn ref-cache
  ([]
     (ref-cache {}))
  ([initial-vals]
     (ref initial-vals)))

(defn ref-cache-get
  [key cache]
  (@cache key))

(defn ref-cache-put
  [key value cache]
  (dosync (alter cache assoc key value)))
