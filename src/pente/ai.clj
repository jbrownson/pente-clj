(ns pente.ai
  (:require [pente.model :as pente]))

(defn minimax [[opposite-player movegen heuristic next-pos :as f] pos depth player]
  "http://en.wikipedia.org/wiki/Minimax"
  (let [moves (movegen pos player)]
    (if (or (empty? moves) (zero? depth))
      [(heuristic pos player) nil]
      (apply max-key first
             (cons
               [Float/NEGATIVE_INFINITY nil]
               (map (fn [move] [(- (first (minimax f (next-pos pos move) (dec depth) (opposite-player player)))) move])
                    moves))))))

(defn pente-minmax [game depth]
  (minimax
    [pente/not-color ; opposite-player
     (fn [game player] ; movegen
       (let [valid-moves (filter (partial pente/valid-move? game) (pente/board-coords (:board game)))]
         (filter (fn [coord]
                   (some (complement nil?)
                         (map
                           (partial pente/cell (:board game))
                           (mapcat (fn [direction]
                                     (take 2 (rest (pente/coords-in-direction (:board game) coord direction))))
                                   pente/directions))))
                 valid-moves)))
     (fn [game color] ; heuristic
       ({nil 0 (pente/not-color color) 1 color -1} (:winner game)))
     pente/move ; next-pos
     ]
    game depth (pente/not-color (:turn game))))