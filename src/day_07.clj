(ns day-07
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input-string
  "pbga (66)
  xhth (57)
  ebii (61)
  havc (66)
  ktlj (57)
  fwft (72) -> ktlj, cntj, xhth
  qoyq (66)
  padx (45) -> pbga, havc, qoyq
  tknk (41) -> ugml, padx, fwft
  jptl (61)
  ugml (68) -> gyxo, ebii, jptl
  gyxo (61)
  cntj (57)"
  )

(defn- get-symbol-list
  "Parses a multi-line string into a listof symbols for parsing
  https://blog.michielborkent.nl/blog/2017/10/10/parsing-a-circuit-with-clojure-spec/"
  [input-string]
  (into [] (comp
            (map (partial format "[%s]"))
            (map edn/read-string))
        (str/split-lines input-string)))

#_(get-symbol-list input-string)
#_(format "[%s]" "havc (66)")


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

;; Sample Conform

#_(s/conform ::program '[jptl (61)])
#_(s/conform ::program '[jptl (61) -> afa, fafdf,  fef])
#_(get-in {:a {:b 2}} [:a :b])
#_(map parse-prog-item '[[jptl (61)]
                         [jptl (61) -> afa, fafdf,  fef]])

#_(->> input-string
       get-symbol-list
       (map parse-prog-item))

(defn add-program-to-tree
  [tower-tree program]
  (let [name (:name program)
        name-key (keyword name)
        is-supported? (name-key tower-tree)
        tower-tree' (if is-supported? tower-tree(assoc tower-tree name-key nil))]
    (if-let [supporting (:supporting program)]
      (let [entry-keys (map keyword supporting)
            entries (mapcat #(vec [% name]) entry-keys)
            tower-tree'' (apply assoc tower-tree' entries)]
        tower-tree'')
      tower-tree)))

#_(add-program-to-tree )

#_(add-program-to-tree {} {:name 'ktlj, :weight 57})
#_(add-program-to-tree {} {:name 'fwft, :weight 72, :supporting '[ktlj cntj xhth]})

(defn get-base
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

(->> input-string
     get-symbol-list
     (map parse-prog-item)
     create-tower
     get-base)
