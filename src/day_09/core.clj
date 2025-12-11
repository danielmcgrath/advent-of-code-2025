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

(defn normalize-rect [[a b] [c d]]
  [[(min a c) (min b d)]
   [(max a c) (max b d)]])

(defn build-edges [points]
  (->> (concat points [(first points)])
       (partition 2 1)
       (map (fn [[p1 p2]] (normalize-rect p1 p2)))
       vec))

(defn rect-valid? [edges [x y] [u v]]
  (not-any?
   (fn [[[p q] [r s]]]
     (and (< p u) (< q v) (> r x) (> s y)))
   edges))

(defn v1 [input]
  (apply max (mapcat identity (all-edges (parse input)))))

(defn v2 [input]
  (let [points (vec (parse input))
        n      (count points)
        edges  (build-edges points)]
    (loop [i 0
           best 0]
      (if (= i n)
        best
        (let [p1 (points i)]
          (recur (inc i)
                 (loop [j    (inc i)
                        best best]
                   (if (= j n)
                     best
                     (let [p2            (points j)
                           [[x y] [u v]] (normalize-rect p1 p2)
                           size          (* (inc (- u x))
                                            (inc (- v y)))]
                       (if (<= size best)
                         (recur (inc j) best)
                         (if (rect-valid? edges [x y] [u v])
                           (recur (inc j) size)
                           (recur (inc j) best))))))))))))
