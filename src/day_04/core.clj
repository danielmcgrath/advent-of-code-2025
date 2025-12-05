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

(defn at-positions [grid]
  (for [r (range (count grid))
        c (range (count (first grid)))
        :let [pos [r c]]
        :when (= \@ (get-in grid pos))]
    pos))

(defn degree-map [grid]
  (into {}
        (for [pos (at-positions grid)]
          [pos (neighboring-roll-count grid pos)])))

(def empty-q clojure.lang.PersistentQueue/EMPTY)

(defn max-clearable-rolls [grid]
  (let [deg0 (degree-map grid)
        ;; start with all '@' that are initially < 4
        q0   (into empty-q
                   (for [[pos d] deg0
                         :when (< d 4)]
                     pos))]
    (loop [deg           deg0
           cleared       #{}
           q             q0
           cleared-count 0]
      (if (empty? q)
        cleared-count
        (let [pos (peek q)
              q   (pop q)]
          (if (cleared pos)
            ;; already processed, skip
            (recur deg cleared q cleared-count)
            (let [cleared'       (conj cleared pos)
                  cleared-count' (inc cleared-count)
                  ;; update neighbors' degrees
                  [deg' q']
                  (reduce
                   (fn [[deg q] npos]
                     (if (and (= \@ (get-in grid npos))
                              (not (cleared' npos)))
                       (let [old (deg npos)
                             new (dec old)
                             deg (assoc deg npos new)
                              ;; if this neighbor just dropped below 4, enqueue it
                             q   (if (and (>= old 4) (< new 4))
                                   (conj q npos)
                                   q)]
                         [deg q])
                       [deg q]))
                   [deg q]
                   (neighbors grid pos))]
              (recur deg' cleared' q' cleared-count'))))))))

(defn v1 [input]
  (count-movable-rolls (parse input)))

(defn v2 [input]
  (max-clearable-rolls (parse input)))
