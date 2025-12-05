(ns day-04.core
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (str/split-lines input)
       (mapv vec)))

(defn in-bounds? [grid [r c]]
  (let [h (count grid)
        w (count (first grid))]
    (and (<= 0 r) (< r h)
         (<= 0 c) (< c w))))

(def deltas-8
  [[-1 -1] [-1 0] [-1 1]
   [0 -1]        [0 1]
   [1 -1] [1 0] [1 1]])

(defn neighbors [grid [r c]]
  (->> deltas-8
       (map (fn [[dr dc]] [(+ r dr) (+ c dc)]))
       (filter #(in-bounds? grid %))))

(defn neighboring-roll-count [grid [r c]]
  (count (filter #(= \@ (get-in grid %))
                 (neighbors grid [r c]))))

(defn count-movable-rolls [grid]
  (count
   (for [r (range (count grid))
         c (range (count (first grid)))
         :let [pos  [r c]
               cell (get-in grid pos)
               v    (neighboring-roll-count grid pos)]
         :when (and (= \@ cell)
                    (< v 4))]
     pos)))

(defn v1 [input]
  (count-movable-rolls (parse input)))

(defn v2 [input]
  (let [nums (parse input)]
    (apply max nums)))

