(ns day-01)

(defn- char->int [chr] (Integer/parseInt (str chr)))

(defn- str->int-seq [str-seq] (map char->int str-seq))

(defn same-sequence [int-seq]
  (let [circular-end-seq (concat int-seq [(first int-seq)])]
    (loop [[current & rem] circular-end-seq
           last-digit nil
           same-digits []]
      (cond
        ;; Terminus
        (nil? current) same-digits
        ;; Digit equal to last
        (= last-digit current) (recur rem last-digit (conj same-digits current))
        ;; all others
        :else (recur rem current same-digits)))))

(defn seq-sum
  [string-seq]
  (->> string-seq
       str->int-seq
       same-sequence
       (reduce +)))

;; Print cases
(utils/print-expr (seq-sum "1122"))
(utils/print-expr (seq-sum "1111"))
(utils/print-expr (seq-sum "1234"))
(utils/print-expr (seq-sum "91212129"))
