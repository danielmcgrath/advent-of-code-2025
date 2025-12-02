(ns day-02.core
  (:require [clojure.string :as str]))

(defn parse [input]
  (str/split (str/trim-newline input) #","))

(defn bisect [s]
  (let [len (count s)
        mid (quot len 2)]
    [(subs s 0 mid) (subs s mid)]))

(defn is-valid? [id]
  (let [s (str id)]
    (if (odd? (count s))
      true
      (let [[first second] (bisect s)]
        (not= first second)))))

(defn parse-range [s]
  (->> (str/split s #"-")
       (map #(bigint %))))

(defn check-range [acc, v]
  (if (is-valid? v) acc (+ acc v)))

(defn tot [acc, item]
  (let [[start end] (parse-range item)]
    (reduce check-range acc (range start (+ end 1)))))

(defn v1 [input]
  (->> (parse input)
       (reduce tot 0)))

(defn v2 [input]
  (let [nums (parse input)]
    (apply max nums)))

