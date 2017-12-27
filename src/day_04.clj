(ns day-04
  (:require [clojure.string :as str]
            [inputs :refer [day-04] :rename {day-04 input}]))

(defn- all-letters?
  [phrase-list]
  (every? #(re-matches #"[a-zA-Z]+" %) phrase-list))

(defn- has-duplicates?
  [phrase-list]
  (->> (group-by identity phrase-list)
       vals
       (some #(> (count %) 1))))

(defn valid-passphrase?
  [phrase]
  (let [phrase-list (str/split phrase #"\s")]
    (and (all-letters? phrase-list)
         (not (has-duplicates? phrase-list)))))

(as-> input _in
  (str/split _in #"\n")
  (map str/trim _in)
  (filter valid-passphrase? _in)
  (count _in))

#_(valid-passphrase? "a b c d")
#_(valid-passphrase? "a1 b c d")
#_(valid-passphrase? "a b c d a")
#_(valid-passphrase? "a b c d a")

#_(str/split "a b" #"\s")
#_(re-matches #"\w+" "aaaA1")
