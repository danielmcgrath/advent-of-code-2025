(ns day-08.core
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn parse-coordinate [coord]
  (map parse-long (str/split coord #",")))

(defn parse [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map parse-coordinate)
       (vec)))

(defn euclidean-distance [[x1 y1 z1] [x2 y2 z2]]
  (let [dx (- x1 x2)
        dy (- y1 y2)
        dz (- z1 z2)]
    (math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(defn compute-distances [junctions]
  (let [n     (count junctions)
        edges (for [i (range n)
                    j (range (inc i) n)]
                {:i i
                 :j j
                 :dist (euclidean-distance (junctions i) (junctions j))})]
    (sort-by :dist edges)))

(defn initial-circuits [junctions]
  (let [n    (count junctions)]
    {:parent (vec (range n))
     :size   (vec (repeat n 1))}))

(defn find-root [parent id]
  (if (= (parent id) id)
    id
    (find-root parent (parent id))))

(defn build-circuits [junctions k]
  (reduce
   (fn [acc edge]
     (let [{:keys [i j]} edge
           parent (:parent acc)
           size (:size acc)
           ri (find-root parent i)
           rj (find-root parent j)]
       (if (= ri rj)
         acc
         (let [[small large] (if (<= (size ri) (size rj))
                               [ri rj]
                               [rj ri])]

           {:parent (assoc parent small large)
            :size   (assoc size large (+ (size large) (size small)))}))))
   (initial-circuits junctions)
   (take k (compute-distances junctions))))

(defn largest-circuits [{:keys [parent size]}]
  (->> (range (count parent))
       (filter #(= (parent %) %))
       (map size)
       (sort >)))

(defn v1 [input]
  (let [junctions (parse input)
        circuits  (build-circuits junctions 1000)
        by-size   (largest-circuits circuits)]
    (reduce * (take 3 by-size))))
