(ns day-06
  (:require [utils :refer [clean-input]]))

(def input "4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5")

(defn- get-max
  [input]
  (let [max-num (apply max input)]
    {:val max-num
     :idx (.indexOf input max-num)}))

(defn- get-block-distribution
  [idx val item-count]
  (->> (range (inc idx) (+ idx val 1))
       (map #(mod % item-count))
       (group-by identity)))

(defn redis-blocks
  [input]
  (loop [memory-bank input
         input-record #{}
         cycle 0]
    (if (get input-record memory-bank)
      cycle
      (let [{:keys [val idx]} (get-max memory-bank)
            block-allocs  (get-block-distribution idx val (count memory-bank))
            updated-memory-bank (map-indexed
                                 (fn [idx itm]
                                   (+ itm (count (block-allocs idx))))
                                 (assoc memory-bank idx 0))]
        (recur (vec updated-memory-bank)
               (conj input-record memory-bank)
               (inc cycle))))))

(def cleaned-input (clean-input input #"\s+"))

(redis-blocks cleaned-input) ; part 1
(redis-blocks [0 2 7 0])
