(ns pente.ai
  (:require [pente.model :as pente]))

; TODO multi-threading

; TODO alpha-beta pruning http://en.wikipedia.org/wiki/Alpha–beta_pruning or other algorithms like negascout
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
  (second (minimax
            [pente/not-color ; opposite-player
             (fn [game player] ; movegen
               (let [valid-moves (filter (partial pente/valid-move? game) (pente/board-coords (:board game)))]
                 (shuffle (filter (fn [coord]
                                    (some (complement nil?)
                                          (map
                                            (partial pente/cell (:board game))
                                            (mapcat (fn [direction]
                                                      (take 2 (rest (pente/coords-in-direction (:board game) coord direction))))
                                                    pente/directions))))
                                  valid-moves))))
             (fn [game color] ; heuristic
               (cond
                 (= color (:winner game)) Float/NEGATIVE_INFINITY
                 (= (pente/not-color color) (:winner game)) Float/POSITIVE_INFINITY
                 :else (+ (- (-> game :captures color)) ((pente/not-color color) (:captures game))))) ; TODO improving this would vastly improve the AI
             pente/move ; next-pos
             ]
            game depth (pente/not-color (:turn game)))))