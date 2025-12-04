(ns day-03.core
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (map Long/parseLong (str/split line #"")))

(defn parse [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map parse-line)
       vec))

(defn find-best-index
  [digits start last-start]
  (loop [i      start
         best-i start]
    (if (> i last-start)
      best-i
      (if (pos? (compare (nth digits i) (nth digits best-i)))
        (recur (inc i) i)
        (recur (inc i) best-i)))))

(defn best-subseq
  [digits k]
  (let [n (count digits)]
    (loop [start 0
           k     k
           acc   []]
      (if (zero? k)
        acc
        (let [last-start (- n k)
              j          (find-best-index digits start last-start)
              acc'       (conj acc (nth digits j))]
          (recur (inc j) (dec k) acc'))))))

(defn v1 [input]
  (->> (parse input)
       (map #(Long/parseLong (apply str (best-subseq % 2))))
       (reduce +)))

(defn v2 [input]
  (->> (parse input)
       (map #(Long/parseLong (apply str (best-subseq % 12))))
       (reduce +)))
