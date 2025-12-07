(ns day-07.core
  (:require [clojure.string :as str]))

(defn parse-grid [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (mapv vec)))

(defn find-char [row ch]
  (->> row
       (keep-indexed (fn [i c] (when (= c ch) i)))
       first))

(defn handle-row [[bits splits] row]
  (reduce-kv
   (fn [[bits splits] idx ch]
     (if (and (= ch \^) (= 1 (bits idx)))
       (let [bits' (-> bits
                       (assoc idx 0)
                       (cond-> (> idx 0)
                         (assoc (dec idx) 1))
                       (cond-> (< (inc idx) (count bits))
                         (assoc (inc idx) 1)))]
         [bits' (inc splits)])
       [bits splits]))
   [bits splits]
   row))

(defn v1 [input]
  (let [grid      (parse-grid input)
        first-row (first grid)
        start-idx (find-char first-row \S)
        bits      (assoc (vec (repeat (count first-row) 0)) start-idx 1)]
    (reduce handle-row [bits 0] grid)))
