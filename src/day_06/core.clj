(ns day-06.core
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map #(str/split (str/trim %) #"\s+"))
       (apply map vector)
       (mapv vec)))

(defn char-grid [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (mapv vec)))

(defn blank-col? [grid idx]
  (every? #(= \space (nth % idx)) grid))

(defn column-count [grid]
  (count (first grid)))

(defn column-chars [grid idx]
  (mapv #(nth % idx) grid))

(defn blocks [grid]
  (->> (range (column-count grid))
       (partition-by #(blank-col? grid %))
       (remove #(blank-col? grid (first %)))))

(defn parse-block [grid col-idxs]
  (let [cols (map #(column-chars grid %) col-idxs)
        op (->> cols
                (keep #(let [c (peek %)]
                         (when (not= c \space) c)))
                first
                str)
        nums (for [col (reverse cols)]
               (->> (butlast col)
                    (remove #(= % \space))
                    (apply str)))]
    (conj (vec nums) op)))

(defn transpose [input]
  (let [grid (char-grid input)]
    (mapv #(parse-block grid %) (blocks grid))))

(defn compute [col]
  (let [op (peek col)
        ns (map parse-long (pop col))]
    (apply (resolve (symbol op)) ns)))

(defn v1 [input]
  (reduce + (map compute (parse input))))

(defn v2 [input]
  (->> (transpose input)
       (map compute)
       (reduce +)))

