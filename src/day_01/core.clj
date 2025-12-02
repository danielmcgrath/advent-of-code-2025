(ns day-01.core
  (:require [clojure.string :as str]))

(defn- split-alpha-numeric [s]
  (let [[_ letters digits] (re-matches #"([A-Za-z]+)(\d+)" s)]
    [letters (Integer/parseInt digits)]))

(defn- move [start [dir steps]]
  (mod (case dir
    "R" (+ start steps)
    "L" (- start steps)) 100))

(defn- parse [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map split-alpha-numeric)
       vec))

(defn- count-zeroes [acc item]
  (let [acc' (update acc :current move item)]
    (cond-> acc'
      (zero? (:current acc')) (update :zeroes inc))))

(defn v1 [input]
  (let [cmds (parse input)]
    (reduce count-zeroes {:current 50 :zeroes 0} cmds)))

(defn v2 [input]
  (let [nums (parse input)]
    (apply max nums)))

