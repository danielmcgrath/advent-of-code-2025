(ns day-09.core
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> input
       (str/split-lines)
       (remove str/blank?)
       (map #(str/split % #","))
       (map #(map parse-long %))))

(defn edge [a b]
  (inc (abs (- a b))))

(defn rsize [[x1 y1] [x2 y2]]
  (* (edge x1 x2) (edge y1 y2)))

(defn edges [[x y] tiles]
  (map #(rsize [x y] %) tiles))

(defn all-edges [tiles]
  (map #(edges % tiles) tiles))

(defn v1 [input]
  (apply max (mapcat identity (all-edges (parse input)))))
