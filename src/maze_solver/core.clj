(ns maze-solver.core)

(def S
  "Constant representing the start of the maze."
  2)

(def F
  "Constant representing the goal."
  3)


(defn cell [maze position]
  (get-in maze position))


(defn empty-space?
  "True if within `maze` `position` is an empty space. False otherwise."
  [maze position]
  (= 0 (cell maze position)))


(defn goal?
  "True if within `maze` `position` is the goal. False otherwise."
  [maze position]
  (= F (cell maze position)))


(defn neighbors
  "Returns the Von Neumann neighbors of `[x y]`."
  [[x y]]
  (for [dx    [-1 0 1]
        dy    [-1 0 1]
        :when (not= (Math/abs dy) (Math/abs dx))]
    [(+ x dx) (+ y dy)]))


(defn possible-movements
  "Returns the neighbors of `position` that are either the goal or an
  empty space."
  [maze position]
  (filter #(or (goal? maze %)
               (empty-space? maze %))
          (neighbors position)))


(defn walk
  "Returns a maze where `position` is marked as part of th path. "
  [maze position]
  (update-in maze position
             (fn [v]
               (if (= 0 v) 4 v))))


(defn solve
  "Given `maze` and the `start-position`, returns a new maze with a path
  from `start-position` to the goal marked by `4`s. Returns `nil` if
  no such path exists."
  [maze start-position]
  (cond
    (goal? maze start-position)
    maze

    (empty? (possible-movements maze start-position))
    nil

    :else
    (->> start-position
         (possible-movements maze)
         (map #(solve (walk maze start-position) %))
         (drop-while nil?)
         first)))
