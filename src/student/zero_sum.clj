(ns student.zero-sum)

(defn make-accounts
  "Create a map of account-num->:initial-balance for :count numbered
  accounts."
  [{:keys [count initial-balance]}]
  (zipmap (range count) (repeatedly (partial ref initial-balance))))

(defn total-balance
  [accounts]
  (apply + (map deref (vals accounts))))

(defn transfer
  [{:keys [accounts from to amount]}]
  (dosync
   (alter (accounts from) - amount)
   (alter (accounts to) + amount)
   )
  )

(defn random-account-ids
  [num-accounts]
  (repeatedly (partial rand-nth (range num-accounts))))

(defn balance
  [accounts account-id]
  @(accounts account-id))

(defn random-transfer
  [accounts]
  (let [[from to] (random-account-ids (count accounts))]
    (println "from:" from "to:" to)
    (transfer {:accounts accounts :from from :to to :amount (rand-int (balance accounts from))})))


