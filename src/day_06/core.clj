(ns day-06.core
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map #(str/split (str/trim %) #"\s+"))
       (apply map vector)
       (mapv vec)))

(defn compute [col]
  (let [op (peek col)
        ns (map parse-long (pop col))]
    (apply (resolve (symbol op)) ns)))

(defn v1 [input]
  (reduce + (map compute (parse input))))

(defn v2 [input]
  (let [nums (parse input)]
    (apply max nums)))

