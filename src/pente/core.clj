(ns pente.core
  (:gen-class)
  (:require [pente.model :as pente]
            [seesaw.core :as seesaw]
            [tailrecursion.javelin-clj :as javelin]))

(seesaw/native!)

(def gap 1)
(def initial-square-size 40)

(javelin/defc game (pente/new-game 13))

(def color-from-cell {:black :black :white :white nil :gray})

(defn create-label [cell coord]
  (let [l (seesaw/label :background (color-from-cell cell) :preferred-size [initial-square-size :by initial-square-size])]
    (seesaw/listen l :mouse-clicked
            (fn [e]
              (swap! game #(let [new-game (pente/move % coord)] (if (nil? new-game) % new-game)))))
    l))

(defn create-labels [board] (map #(create-label (pente/cell board %) %) (sort (fn [[ax ay] [bx by]] (compare [ay ax] [by bx])) (pente/board-coords board))))

(def f (seesaw/frame :title "Pente" :on-close :exit))

(javelin/cell=
  (seesaw/config! f :content
                  (seesaw/grid-panel :background :black
                                     :columns (count (:board game))
                                     :items (create-labels (:board game))
                                     :vgap gap :hgap gap)))

(defn -main []
  (-> f seesaw/pack! seesaw/show!))