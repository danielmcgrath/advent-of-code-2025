(ns day-10.core
  (:require [clojure.string :as str])
  (:import [org.ojalgo.optimisation ExpressionsBasedModel]))

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
     :joltage-requirements  (mapv parse-long (str/split braces #","))}))

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

(defn mask->indices [mask]
  (loop [i 0, acc []]
    (if (< i 64)
      (recur (inc i) (if (bit-test mask i) (conj acc i) acc))
      acc)))

(defn solve-v2 [buttons joltage]
  (let [model (ExpressionsBasedModel.)
        n (count joltage)
        vars (mapv (fn [_]
                     (doto (.addVariable model)
                       (.lower 0)
                       (.integer true)))
                   buttons)]
    (let [obj (.addExpression model "objective")]
      (doseq [v vars]
        (.set obj v 1))
      (.weight obj 1))
    (doseq [i (range n)]
      (let [constraint (.addExpression model (str "eq" i))
            target (nth joltage i)]
        (doseq [[button-idx button] (map-indexed vector buttons)]
          (when (some #(= i %) (mask->indices button))
            (.set constraint (nth vars button-idx) 1)))
        (.level constraint target)))
    (when-let [result (.minimise model)]
      (.getValue result))))

(defn v1 [input]
  (reduce + (map (fn [{:keys [target buttons]}] (min-presses 0 target buttons)) (parse input))))

(defn v2 [input]
  (reduce + (map (fn [{:keys [buttons joltage-requirements]}]
                   (or (solve-v2 buttons joltage-requirements) 0))
                 (parse input))))
