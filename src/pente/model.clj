(ns pente.model)

(defn new-game [size]
  {:board    (assoc-in (vec (repeat size (vec (repeat size nil))))
                       (repeat 2 (int (Math/floor (/ size 2))))
                       :white)
   :turn     :black
   :captures {:white 0 :black 0}})

(def not-color {:white :black :black :white})

(defn cell [board [x y]] ((board y) x))
(defn valid-coord? [board coord] (every? #(and (<= 0 %) (> (count board) %)) coord))
(def directions (disj (set (for [dx (range -1 2) dy (range -1 2)] [dx dy])) [0 0]))

(defn coords-in-direction [board coord direction]
  (take-while (partial valid-coord? board) (iterate #(map + % direction) coord)))

(defn board-coords [board]
  (mapcat identity (map-indexed (fn [y row] (map-indexed (fn [x cell] [x y]) row)) board)))

(defn winner? [board captures color]
  (letfn [(five-in-a-row?[] (some (fn [coord] ; Lots of efficiency gains to be had here
                                    (some (fn [direction]
                                            (= 5 (count (take-while (partial = color) (map (partial cell board) (take 5 (coords-in-direction board coord direction)))))))
                                          directions))
                                  (board-coords board)))]
    (or
      (<= 5 (color captures))
      (five-in-a-row?))))

(defn valid-move? [game coord]
  (and
    (not (nil? (:turn game)))
    (valid-coord? (:board game) coord)
    (nil? (cell (:board game) coord))))

(defn captures [board color coord]
  (letfn [(capture? [direction] (= (map (partial cell board) (rest (take 4 (coords-in-direction board coord direction)))) (conj (vec (repeat 2 (not-color color))) color)))]
    (map #(rest (take 3 (coords-in-direction board coord %))) (filter capture? directions))))

(defn move [game [x y :as coord]]
  (if (valid-move? game coord)
    (let [captures (captures (:board game) (:turn game) coord)
          new-board (reduce (fn [board [x y]] (assoc-in board [y x] nil))
                            (assoc-in (:board game) [y x] (:turn game))
                            (mapcat identity captures))
          new-captures (update-in (:captures game) [(:turn game)] #(+ % (count captures)))]
      (conj
        {:board    new-board
         :captures new-captures}
        (if (winner? new-board new-captures (:turn game))
          [:winner (:turn game)]
          [:turn (not-color (:turn game))]))
      )
    nil))

(defn play [size moves]
  (reduce move (new-game size) moves)) ; change reduce to reductions to see all of the intermediate results

(def no-moves [5 []])
(def one-move [5 [[0 0]]])
(def black-caps [5 [[0 0] [1 0] [2 1] [2 0] [3 0]]])
(def black-caps-and-wins-by-5 (update-in black-caps [1] #(concat % [[4 4] [1 0] [3 4] [2 0] [2 4] [4 0]])))
(def black-wins-by-capping (update-in black-caps [1] #(concat % [[2 3] [2 4] [1 1] [1 0] [2 2] [3 3] [0 1] [0 4] [0 2] [0 3] [1 2] [0 2] [2 2] [3 2]])))

(apply play no-moves) ; =>
{:board
           [[nil nil nil nil nil]
            [nil nil nil nil nil]
            [nil nil :white nil nil]
            [nil nil nil nil nil]
            [nil nil nil nil nil]],
 :turn :black,
 :captures {:black 0, :white 0}}

(apply play one-move) ; =>
{:turn :white,
 :board
           [[:black nil nil nil nil]
            [nil nil nil nil nil]
            [nil nil :white nil nil]
            [nil nil nil nil nil]
            [nil nil nil nil nil]],
 :captures {:black 0, :white 0}}

(apply play black-caps) ; =>
{:turn :white,
 :board
           [[:black nil nil :black nil]
            [nil nil :black nil nil]
            [nil nil :white nil nil]
            [nil nil nil nil nil]
            [nil nil nil nil nil]],
 :captures {:black 1, :white 0}}

(apply play black-caps-and-wins-by-5) ; =>
{:winner :black,
 :board
           [[:black :black :black :black :black]
            [nil nil :black nil nil]
            [nil nil :white nil nil]
            [nil nil nil nil nil]
            [nil nil :white :white :white]],
 :captures {:black 1, :white 0}}

(apply play black-wins-by-capping) ; =>
{:winner :black,
 :board
           [[:black :black nil :black nil]
            [nil nil :black nil nil]
            [:black nil nil :black nil]
            [:black nil nil :black nil]
            [:black nil :black nil nil]],
 :captures {:black 5, :white 0}}