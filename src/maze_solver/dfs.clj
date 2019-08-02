(ns maze-solver.dfs
  (:require [maze-solver.core :refer [possible-movements]]))


(defn visited?
  "True if `v` appears in `visited`. False otherwise."
  [visited v]
  (boolean (some #{v} visited)))


(defn visit
  "Adds `v` to `visited`, does nothing if it is already there."
  [visited v]
  (if-not (visited? visited v)
    (conj visited v)
    visited))


(defn dfs
  "Returns `[parents dfs-tree]` for `maze`, starting from `position`."
  [maze position]
  (loop [stack [position]
         visited []
         parents {}]
    (if (empty? stack)
      [parents visited]
      (let [v          (peek stack)
            neighbors  (possible-movements maze v)
            discovered (filter #(not (visited? visited %)) neighbors)]
        (recur (into (pop stack) discovered)
               (visit visited v)
               (reduce #(assoc % %2 v) parents discovered))))))


(defn path
  "Returns the path from `start` to `goal`, by following the references in
  `parents`."
  [parents start goal]
  (when (contains? parents goal)
    (loop [v    goal
           path (list)]
      (if (= start v)
        (conj path v)
        (recur (parents v)
               (conj path v))))))


;; (solve (maze-solver.parser/parse-maze "/tmp/example-2.txt"))
(defn solve
  "Solves a maze and returns the path from `start` to `goal`. Returns
  `nil` if no such path exists."
  [{:keys [maze start goal]}]
  (let [[parents] (dfs maze start)]
    (path parents start goal)))
