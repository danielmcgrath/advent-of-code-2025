(ns day-01.core
  (:require [clojure.string :as str]))

(defn- split-alpha-numeric [s]
  (let [[_ letters digits] (re-matches #"([A-Za-z]+)(\d+)" s)]
    [letters (Integer/parseInt digits)]))

(defn- parse [input]
  (->> (str/split-lines input)
       (remove str/blank?)
       (map split-alpha-numeric)
       vec))

(defn- step-delta [[dir steps]]
  (case dir
    "L" (- steps)
    "R" steps))

(defn- move [start item]
  (mod (+ start (step-delta item)) 100))

(defn- zero-hits-forward [prev steps]
  (if (zero? steps)
    0
    (let [dist0 (if (zero? prev)
                  100
                  (- 100 prev))]
      (if (< steps dist0)
        0
        (inc (quot (- steps dist0) 100))))))

(defn- zero-hits-backward [prev steps]
  (if (zero? steps)
    0
    (let [dist0 (if (zero? prev)
                  100
                  prev)]
      (if (< steps dist0)
        0
        (inc (quot (- steps dist0) 100))))))

(defn- wrap-count [prev [dir steps]]
  (case dir
    "R" (zero-hits-forward prev steps)
    "L" (zero-hits-backward prev steps)))

(defn- make-counter [count-wraps?]
  (fn [acc item]
    (let [prev   (:current acc)
          acc'   (update acc :current move item)
          cur    (:current acc')
          hits   (wrap-count prev item)
          delta-zeroes (if count-wraps?
                         hits
                         (if (zero? cur) 1 0))]
      (cond-> acc'
        (pos? delta-zeroes) (update :zeroes + delta-zeroes)))))

(defn v1 [input]
  (let [cmds (parse input)]
    (reduce (make-counter false) {:current 50 :zeroes 0} cmds)))

(defn v2 [input]
  (let [cmds (parse input)]
    (reduce (make-counter true) {:current 50 :zeroes 0} cmds)))

