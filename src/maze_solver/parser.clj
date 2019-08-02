(ns maze-solver.parser
  (:require [clojure.java.io :as io]))


(defn rows-to-maze
  "Takes `rows` a seq of strings (each representing row in the maze) and
  returns a map:

    {:maze [[] [] [] []]
     :start [n m]
     :goal [n' m']}

  With the maze described as a vector of vectors of integers, and the
  start and goal positions."
  [rows]
  (let [conjv (fnil conj [])] ;; same as conj, but defaults to vectors instead of lists
    (reduce-kv (fn [acc row-idx row] ;; row as str
                 (reduce-kv (fn [acc' col-idx char]
                              (case char
                                \S (-> acc'
                                       (assoc :start [row-idx col-idx])
                                       (update-in [:maze row-idx] conjv 2))
                                \F (-> acc'
                                       (assoc :goal [row-idx col-idx])
                                       (update-in [:maze row-idx] conjv 3))
                                (update-in acc'[:maze row-idx] conjv (Character/getNumericValue char))))
                            acc
                            (vec row))) ;; vector of chars
               {:start nil
                :goal  nil
                :maze  []}
               (vec rows)))) ;; start with vector of strings


(defn parse-maze
  "Opens `filename` (which must be in the classpath) and returns the
  maze described in it."
  [filename]
  (with-open [r (io/reader filename)]
    ;; We don't need the size (maybe we did if validating the file...)
    (rows-to-maze
      (drop 2 (line-seq r)))))
