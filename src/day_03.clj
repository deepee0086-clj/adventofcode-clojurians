(ns day-03
  (:require
   [clojure.string :as str]
   [clojure.set :as cset]
   [inputs :refer [day-03] :rename {day-03 input}]))

(def base-adjacent-nums 8)

(defn level-count
  "Returns the number of items for a `level`"
  [level]
  (+ base-adjacent-nums (* level 8)))

(defn level-start-num
  "Returns the first number for a given `level`"
  [level]
  (loop [cur-level level
         count 0]
    (cond
      (= cur-level 0) (+ 2 count)
      :else (let [prev-level-count (level-count (dec cur-level))]
              (recur (dec cur-level) (+ count prev-level-count))))))

(defn get-level
  "Get level info for a number.
  Given a `number`, return map containing the level the number appears,
  the starting number for that level, and the index of the number in the level."
  [number]
  (let [start-seq (map level-start-num (iterate inc 0))
        found-seq (take-while #(<= % number) start-seq)
        start (last found-seq)]
    {:level (dec (count found-seq))
     :start start
     :offset (- number start)}))

;; getting a swinging sequence
(def opts #{inc dec})
(defn- next-num
  [start end]
  (let [op (atom dec)]
    (fn [item]
      (let [next (@op item)]
        (if (<= start next end) next
            (do (swap! op #(first (cset/difference opts #{%})))
                (@op item)))))))

(defn- frequency-sequence
  ([start end] (frequency-sequence start end start))
  ([start end initial]
   (iterate (next-num start end) initial)))

(defn- moves-range
  [level]
  [(inc level) (->> level inc (* 2))])

(defn steps-to-origin
  "Give a num, return how many steps to return to 1 in spiral sequence"
  [num]
  (cond
    (= 1 num) 0
    (> num 1) (let [{:keys [level offset]} (get-level num)
                    [min max] (moves-range level)
                    step-sequence (frequency-sequence min max (dec max))]
                (nth step-sequence offset))))

(steps-to-origin input)
(time (steps-to-origin input))

#_(level-side-length 0)
;; Part 2
(defn- offset-coord [[coord-x coord-y] [offset-x offset-y]]
  [(+ coord-x offset-x) (+ coord-y offset-y)])

(def neighbor-offsets [[0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1]])

(defn- add-surrounding [coord grid]
  (reduce
   (fn [sum offset]
     (if-let [neighbor-val (grid (offset-coord coord offset))]
       (+ sum neighbor-val)
       sum))
   0
   neighbor-offsets))

#_(add-surrounding [0 1] {[0 0] 1
                          [1 0] 1
                          [1 1] 2})

(defn traverse-grid
  [n]
  (loop [grid {[0 0] 1} ;starting grid to use for
         last-val 1
         cur-coord [1 0]
         idx 0
         [cur-offset & rest-offset :as offsets] [[1 0] [0 1] [-1 0] [0 -1]]]
    (if (= current-index n) last-val
        (let [surrounding-sum (add-surrounding (:coord cur-item))]
          (recur (assoc grid cur-coord surrunding-sum)
                 surrounding-sum
                 (offset-coord cur-coord cur-offset)
                 (inc x))))))


#_(loop [[first & rest] '(1 2 3)
         accum 0]
    (if (nil? first)
      accum
      (recur rest (+ accum first))))
;;
#_(let [[first & rest] [[1 0] [0 1] [-1 0] [0 -1]]]
    (conj (into [] rest) first))
