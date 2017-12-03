(ns utils)

(defn format-obj [expr] {:expr `'~expr :value expr})
(defmacro format-expr [expr] (format-obj expr))

(defmacro print-expr [expr] `(print ~(format-obj expr)))

#_(format-expr (first [1 2 3]))
#_(print-expr (first [1 2 3]))
