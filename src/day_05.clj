(ns day-05
  (:require [inputs :refer [day-05] :rename {day-05 input}]
            [clojure.string :as str]))

(def str->int-xform
  (comp
   (map str/trim)
   (map #(Integer/parseInt %))))

(defn- clean-input
  [input-str]
  (as-> input-str _s
    (str/split _s #"\n")
    (into [] str->int-xform _s))) ; Have to wrap java methods in a function

(defn traverse-maze
  [input]
  (loop [maze input
         steps-taken 0
         i 0]
    (if-let [current-val (get maze i)]
      (recur
       (assoc maze i (inc current-val))
       (inc steps-taken)
       (+ i current-val))
      {:steps-taken steps-taken
       :final-maze maze})))

#_(clean-input test-input)
#_(traverse-maze [2 2 -1 -2 0])

;; Solution
(-> input
    clean-input
    traverse-maze
    :steps-taken)
