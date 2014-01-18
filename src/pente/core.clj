(ns pente.core
  (:gen-class)
  (:require [pente.model :as pente]
            [pente.ai :as ai]
            [seesaw.core :as seesaw]
            [tailrecursion.javelin-clj :as javelin]))

; TODO show players' captured pairs count in the GUI

(seesaw/native!)

(def game-size 13)
(def ai-depth 2)
(def gap 1)
(def initial-square-size 40)

(javelin/defc game (pente/new-game game-size))

(def color-from-cell {:black :black :white :white nil :gray})

(defn create-label [cell coord]
  (let [l (seesaw/label :background (color-from-cell cell) :preferred-size [initial-square-size :by initial-square-size])]
    (seesaw/listen l :mouse-clicked
                   (fn [e]
                     (swap! game #(let [new-game (pente/move % coord)] (if (nil? new-game) % new-game)))))
    l))

(defn create-labels [board] (map #(create-label (pente/cell board %) %) (sort (fn [[ax ay] [bx by]] (compare [ay ax] [by bx])) (pente/board-coords board))))

(def f (let [new-item (seesaw/menu-item :text "New")
             ai-item (seesaw/menu-item :text "AI Move")]
         (seesaw/listen new-item :action (fn [e] (reset! game (pente/new-game game-size))))
         (seesaw/listen ai-item :action (fn [e] (swap! game #(let [move (ai/pente-minimax % ai-depth)] (pente/move % move)))))
         (seesaw/frame :title "Pente" :on-close :exit :menubar (seesaw/menubar :items [(seesaw/menu :text "Game" :items [new-item ai-item])]))))

(javelin/cell=
  (seesaw/config! f :content
                  (seesaw/grid-panel :background :black
                                     :columns (count (:board game))
                                     :items (create-labels (:board game))
                                     :vgap gap :hgap gap)))

(defn -main []
  (-> f seesaw/pack! seesaw/show!))