(ns day-05
  (:require [inputs :refer [day-05] :rename {day-05 input}]
            [utils :only [clean-input]]
            [clojure.string :as str]))


(defn traverse-maze
  [input offset-fn]
  (loop [maze input
         steps-taken 0
         i 0]
    (if-let [current-val (get maze i)]
      (recur
       (assoc maze i (offset-fn current-val))
       (inc steps-taken)
       (+ i current-val))
      {:steps-taken steps-taken
       :final-maze maze})))

#_(clean-input test-input)
#_(traverse-maze [2 2 -1 -2 0])

;; Solution
(-> input
    (clean-input )
    (traverse-maze inc) ; part 1
    (traverse-maze #(if (>= % 3)
                      (dec %)
                      (inc %))) ; part 2
    :steps-taken)
