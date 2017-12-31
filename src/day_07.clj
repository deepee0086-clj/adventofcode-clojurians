(ns day-07
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [utils :refer [get-symbol-list]]))

(s/def ::prog-name symbol?)
(s/def ::prog-weight (s/and (s/coll-of int)
                            #(= 1 (count %))))

(s/def ::supported-list (s/* ::prog-name))
(s/def ::supporting (s/? (s/cat :arrow #{'->}
                                :programs ::supported-list)))

(s/def ::program (s/cat :name ::prog-name
                        :weight ::prog-weight
                        :supporting ::supporting))

(defn parse-prog-item
  [prog-item]
  (let [parsed (s/conform ::program prog-item)
        weight (first (:weight parsed))
        supported (get-in parsed [:supporting :programs])]
    (-> {:name (:name parsed)
         :weight weight}
        (conj (when supported [:supporting supported])))))

(defn add-program-to-tree
  [tower-tree program]
  (let [name (:name program)
        name-key (keyword name)
        is-supported? (name-key tower-tree)
        tower-tree' (if is-supported?
                      tower-tree
                      (assoc tower-tree name-key nil))]
    (if-let [supporting (:supporting program)]
      (let [entry-keys (map keyword supporting)
            entries (mapcat #(vec [% name]) entry-keys)
            tower-tree'' (apply assoc tower-tree' entries)]
        tower-tree'')
      tower-tree)))

(defn get-base-program
  [tower-tree]
  (->> tower-tree
       (filter #(nil? (%1 1)))
       first
       first))

(defn create-tower
  [input]
  (loop [[item & rest] input
         tower-tree {}]
    (if (nil? item)
      tower-tree
      (recur rest
             (add-program-to-tree tower-tree item)))))

;; Find base element
(->> (slurp "src/input/day_07.txt")
     get-symbol-list
     (map parse-prog-item)
     create-tower
     get-base-program)
