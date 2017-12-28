(ns utils
  (:require [clojure.string :as str]))

(defn format-obj [expr] {:expr `'~expr :value expr})
(defmacro format-expr [expr] (format-obj expr))

(defmacro print-expr [expr] `(print ~(format-obj expr)))

(def str->int-xform
  (comp
   (map str/trim)
   (map #(Integer/parseInt %))))

(defn clean-input
  ([input-str] (clean-input input-str #"\n+"))
  ([input-str separator]
   (as-> input-str _input
     (str/split _input separator)
     (into [] str->int-xform _input)))) ; Have to wrap java methods in a function
#_(format-expr (first [1 2 3]))
#_(print-expr (first [1 2 3]))
