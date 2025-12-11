(ns day-11.core
  (:require [clojure.string :as str]))

(defn parse [input]
  (into {}
        (for [line (str/split-lines input)
              :let [[node & neighbors] (str/split line #"[: ]+")]
              :when node]
          [node (vec neighbors)])))

(defn count-paths [graph start end]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start #{}])
         path-count 0]
    (if-let [[node visited] (peek queue)]
      (cond
        (= node end)
        (recur (pop queue) (inc path-count))

        (visited node)
        (recur (pop queue) path-count)

        :else
        (let [neighbors (get graph node [])
              new-visited (conj visited node)
              new-queue (into (pop queue)
                              (map #(vector % new-visited) neighbors))]
          (recur new-queue path-count)))
      path-count)))

(defn count-paths-through [graph start end required-nodes]
  (let [required-set (set required-nodes)
        memo (atom {})]
    (letfn [(dfs [node visited seen-required]
              (let [state [node seen-required]]
                (if-let [cached (@memo state)]
                  cached
                  (let [result
                        (cond
                          (visited node) 0

                          (and (= node end)
                               (= seen-required required-set)) 1

                          (= node end) 0

                          :else
                          (let [new-visited (conj visited node)
                                new-seen (if (required-set node)
                                           (conj seen-required node)
                                           seen-required)]
                            (reduce +
                                    (map #(dfs % new-visited new-seen)
                                         (get graph node [])))))]
                    (swap! memo assoc state result)
                    result))))]
      (dfs start #{} #{}))))

(defn v1 [input]
  (count-paths (parse input) "you" "out"))

(defn v2 [input]
  ;; note: this doesn't work with the test input, because it starts at "you"
  ;; rather than at "svr"
  (count-paths-through (parse input) "svr" "out" ["fft" "dac"]))

