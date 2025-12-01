(ns aoc.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn- parse-args [args]
  (loop [opts {:test? false}
         [flag & more] args]
    (if (nil? flag)
      opts
      (case flag
        "--day"   (recur (assoc opts :day (Integer/parseInt (first more))) (rest more))
        "--stage" (recur (assoc opts :stage (Integer/parseInt (first more))) (rest more))
        "--test"  (recur (assoc opts :test? true) more)
        (throw (ex-info (str "Unknown flag: " flag) {:flag flag}))))))

(defn- day-dir [day]
  (format "src/day_%02d" day))

(defn- solver-fn [day stage]
  (let [ns-str (format "day-%02d.core" day)
        fn-name (case stage
                  1 "v1"
                  2 "v2"
                  (throw (ex-info (str "Bad stage " stage) {:stage stage})))
        sym (symbol ns-str fn-name)
        f   (requiring-resolve sym)]
    (when-not f
      (throw (ex-info (str "No solver for " ns-str " stage " stage)
                      {:ns ns-str :stage stage})))
    f))

(defn- input-path [day test?]
  (let [dir   (day-dir day)
        fname (if test? "input.test.txt" "input.txt")]
    (io/file dir fname)))

(defn -main [& args]
  (let [{:keys [day stage test?]} (parse-args args)
        _ (when (or (nil? day) (nil? stage))
            (throw (ex-info "Must supply --day and --stage" {})))

        solver (solver-fn day stage)

        file   (input-path day test?)
        _      (when-not (.exists ^java.io.File file)
                 (throw (ex-info "Input file not found"
                                 {:path (.getPath file)})))
        input  (slurp file)

        start  (System/nanoTime)
        result (solver input)
        end    (System/nanoTime)
        ms     (/ (double (- end start)) 1e6)]

    (println result)
    (binding [*out* *err*]
      (println (format "[day %02d stage %d%s] %.2f ms"
                       day stage (if test? " (test)" "") ms)))))
