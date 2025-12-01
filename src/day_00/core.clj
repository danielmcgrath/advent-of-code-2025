;; This is just a test file for playing with scaffolding, not a real challenge.

(ns day-00.core
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map #(Integer/parseInt %))
       vec))

(defn v1 [input]
  (let [nums (parse input)]
    (reduce + nums)))

(defn v2 [input]
  (let [nums (parse input)]
    (apply max nums)))

