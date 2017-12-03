(ns day-02
  (:require utils
            [clojure.string :as str]))

(defn- str->row [row]
  (->> (str/split row #"\s")
       (map #(Integer/parseInt %))))

(defn- str->spreadsheet [input]
  (let [str-rows (str/split-lines input)]
    (map str->row str-rows)))

(defn- check-bounds [candidate current-bound op]
  (if (or (nil? current-bound) (op candidate current-bound))
    candidate
    current-bound))

(defn- check-min [candidate min] (check-bounds candidate min <))

(defn- check-max [candidate max] (check-bounds candidate max >))

(defn row-checksum [spreadsheet-row]
  (loop [[first & rest] spreadsheet-row
         min nil
         max nil]
    (if (nil? first)
      (- max min)
      (recur rest (check-min first min) (check-max first max)))))

(defn get-checksum [input]
  (let [spreadsheet (str->spreadsheet input)]
    (->> spreadsheet
         (map row-checksum)
         (reduce +))))
