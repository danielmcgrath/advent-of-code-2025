(ns day-05.core
  (:require [clojure.string :as str]))

(defn split-input [v]
  (let [[before after] (split-with (partial not= "") v)]
    [before (rest after)]))

(defn parse-range [s]
  (let [[a b] (str/split s #"-")]
    [(parse-long a) (parse-long b)]))

(defn collapse-ranges [vr]
  (let [ranges (sort-by first (map parse-range vr))]
    (->> ranges
         (reduce
          (fn [acc [lo hi]]
            (let [[curr-lo curr-hi] (peek acc)]
              (if (<= lo (inc curr-hi))
                (conj (pop acc) [curr-lo (max curr-hi hi)])
                (conj acc [lo hi]))))
          [(first ranges)]))))

(defn in-range? [id [lo hi]]
  (<= lo id hi))

(defn valid? [ranges id]
  (some (partial in-range? id) ranges))

(defn check-ids [ranges ids]
  (count (filter (partial valid? ranges) ids)))

(defn parse [input]
  (let [[ranges ids] (split-input (str/split-lines input))]
    [(collapse-ranges ranges) (map parse-long ids)]))

(defn v1 [input]
  (let [[ranges ids] (parse input)]
    (check-ids ranges ids)))

(defn v2 [input]
  (let [nums (parse input)]
    (apply max nums)))

