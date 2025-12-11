(ns day-10.core
  (:require [clojure.string :as str]))

(defn light->mask [s]
  (reduce
   (fn [m [idx ch]]
     (if (= ch \#)
       (bit-set m idx)
       m))
   0
   (map-indexed vector (str s))))

(defn button->mask [s]
  (let [nums (->> (re-seq #"\d+" s)
                  (map #(parse-long %)))]
    (reduce bit-set 0 nums)))

(defn parse-line [line]
  (when-let [[_ bracket parens braces]
             (re-matches #"^\s*\[([^\]]+)\]\s*((?:\([^)]*\)\s*)*)\{([^}]*)\}\s*$"
                         line)]
    {:target                (light->mask bracket)
     :buttons               (mapv button->mask (re-seq #"\([^)]*\)" parens))
     :joltage-requirements  braces}))

(defn parse [input]
  (->> input
       (str/split-lines)
       (remove str/blank?)
       (map parse-line)))

(defn neighbors [state buttons]
  (map #(bit-xor state %) buttons))

(defn min-presses [start target buttons]
  (loop [queue   (conj clojure.lang.PersistentQueue/EMPTY [start 0])
         visited #{start}]
    (if-let [[state steps] (peek queue)]
      (cond
        (= state target)
        steps

        :else
        (let [queue' (pop queue)
              [q v]
              (reduce
               (fn [[q vs] next-state]
                 (if (vs next-state)
                   [q vs]
                   [(conj q [next-state (inc steps)])
                    (conj vs next-state)]))
               [queue' visited]
               (neighbors state buttons))]
          (recur q v)))
      nil)))

(defn v1 [input]
  (reduce + (map (fn [{:keys [target buttons]}] (min-presses 0 target buttons)) (parse input))))
