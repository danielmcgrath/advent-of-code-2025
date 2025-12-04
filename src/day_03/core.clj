(ns day-03.core
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map #(str/split % #""))
       vec))

(defn pair-val [a b]
  (Long/parseLong (str a b)))

(defn line-max [digits]
  (loop [[head & tail] digits
         best Long/MIN_VALUE]
    (if (nil? head)
      best
      (let [head-best (if (seq tail)
                        (apply max best (map #(pair-val head %) tail))
                        best)]
        (recur tail head-best)))))

(defn max-combos [lines]
  (mapv line-max lines))

(defn v1 [input]
  (->> (parse input)
       (max-combos)
       (reduce +)))

(defn v2 [input]
  (let [nums (parse input)]
    (apply max nums)))

