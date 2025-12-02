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

(defn is-all-repeating? [s]
  (let [n (count s)]
    (loop [len 1]
      (if (> len (/ n 2))
        false
        (let [sub (subs s 0 len)]
          (if (and (zero? (mod n len))
                   (= s (apply str (repeat (/ n len) sub))))
            true
            (recur (inc len))))))))

(defn parse-range [s]
  (map bigint (str/split s #"-")))

(defn check-range [acc v]
  (if (is-valid? v) acc (+ acc v)))

(defn check-range-2 [acc v]
  (if (is-all-repeating? (str v)) (+ acc v) acc))

(defn sum-range [rf acc item]
  (let [[start end] (parse-range item)]
    (reduce rf acc (range start (inc end)))))

(defn v1 [input]
  (->> (parse input)
       (reduce (partial sum-range check-range) 0)))

(defn v2 [input]
  (->> (parse input)
       (reduce (partial sum-range check-range-2) 0)))
