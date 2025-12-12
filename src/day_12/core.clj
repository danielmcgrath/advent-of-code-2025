(ns day-12.core
  (:require [clojure.string :as str]))

(defn split-blocks [input]
  (->> (str/split input #"\R\s*\R+")
       (map str/trim)
       (remove str/blank?)))

(defn parse-shape-block [block]
  (let [[hdr & rows] (str/split-lines block)
        [_ id-str] (re-matches #"^\s*(\d+)\s*:\s*$" hdr)]
    [(parse-long id-str) (vec rows)]))

(defn parse-region-line [line]
  (let [[_ w-str h-str nums-str] (re-matches #"^\s*(\d+)\s*x\s*(\d+)\s*:\s*(.*)\s*$" line)
        nums (->> (str/split nums-str #"\s+")
                  (mapv parse-long))
        counts (->> nums
                    (map-indexed vector)
                    (remove (fn [[_ n]] (zero? n)))
                    (into {}))]
    {:w (parse-long w-str)
     :h (parse-long h-str)
     :counts counts}))

(defn parse-input [input]
  (let [blocks (split-blocks input)
        shapes (->> (butlast blocks)
                    (map parse-shape-block)
                    (into {}))
        cases  (->> (last blocks)
                    (str/split-lines)
                    (remove str/blank?)
                    (mapv parse-region-line))]
    {:shapes shapes
     :cases cases}))

(defn region-is-large-enough? [{:keys [w h counts]}]
  (let [board-area (* w h)
        num-shapes (reduce + (vals counts))
        needed (* 9 num-shapes)] ; 9 is the max area of all shapes in the input
    (<= needed board-area)))

(defn check-cases [input]
  (let [{:keys [cases]} (parse-input input)]
    (->> cases
         (filter region-is-large-enough?)
         vec)))

(defn v1 [input]
  (count (check-cases input)))
