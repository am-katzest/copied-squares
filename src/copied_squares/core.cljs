(ns copied-squares.core
  (:require [quil.core :as q :include-macros true]
            [copied-squares.simulation :refer [update-simulation] :as sim]
            [copied-squares.gui :as gui]
            [copied-squares.drawing :as draw]
            [copied-squares.types :refer [make-xy xy* xy+ xydist ->ball]]
            [copied-squares.statistics :as stat]
            [copied-squares.state :refer [sizex sizey] :as state]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [quil.middleware :as m]
            [copied-squares.state :as s]))

(def initial-balls
  [{:color :18 :count 1 :radius 0.5 :speed 0.5}
   {:color :70 :count 1 :radius 0.5 :speed 0.5}
   {:color :150 :count 1 :radius 0.5 :speed 0.5}])

(defn random-ball []
  {:color (-> 256 rand-int str keyword)
   :count 1 :radius 0.5 :speed 0.5})

(def gui-size (r/atom {:x 20 :y 20}))   ; size of board (to be set on restart)
(def gui-ball-editor-state (r/atom initial-balls)) ; description of balls to be created when restarting simulation
(def last-state (atom nil))           ; saved state from last iteration (used to resume simulation in another quil instance)
(def initial-squares (atom nil))         ; function used to generate inital squares
(def paint-over-ball-shadows (atom nil)) ; function used to cover balls' tracks
(def draw-balls? (atom true))
(def overlay (atom nil))
(def redraw-queued? (atom true))        ; setting this to true causes everything to be drawn again
(def target-frame-rate (atom 15))
(def current-frame-rate (r/atom 15))
(def every-second (atom true))          ; is briefly set to true every second (for one quil cycle)

(defn initial-squares-gray [_balls]
  (vec (repeat (* sizey sizex) :gray)))

(defn initial-squares-closest [balls]
  (vec (for [i (range (* sizey sizex))]
         (let [center (xy+ (state/inverse-coord i) (make-xy 0.5 0.5))]
           (:color (apply min-key #(xydist center (:position %)) balls))))))

(defn rand-position [color vel size & {:keys [angle]}]
  (let [color (if (int? color) (keyword (str color)) color)
        position (make-xy (rand-int sizex) (rand-int sizey))
        angle (or angle (rand (* 2.0 Math/PI)))
        velocity (xy* (make-xy (Math/sin angle) (Math/cos angle)) vel)]
    (->ball color position velocity size)))

(defn hex "converts a number (0..255) to hexadecimal" [n]
  (let [lookup "0123456789abcdef"]
    (str (nth lookup (quot n 16)) (nth lookup (mod n 16)))))

(defn color->rgb [k]
  (->> k
       (draw/wall-colors)               ;lookup color
       (apply q/color)                  ;create quil color object
       ((juxt q/red q/green q/blue))    ;get r g b
       (map (comp hex int))             ;convert to hex
       (apply str "#")))             ;concat

(defn update-color-palette []
  ;; it must be called in one of "quil" functions, so i cannot just
  (reset! s/rgb-colors
          (into {}
                (for [i (range 256)]
                  (let [k (keyword (str i))]
                    [k (color->rgb k)])))))
;; TODO penetration chance
(defn generate-balls []
  (vec (for [{:keys [color count radius speed]} @gui-ball-editor-state
             _count (range count)]
         (rand-position color speed radius))))

(defn generate-new-state []
  (let [balls (generate-balls)]
    (->
     {:frame 0
      :color-history []
      :squares (@initial-squares balls)
      :clearlist-deltas {}
      :clearlist {}
      :balls balls}
     sim/create-clearlists
     stat/count-colors
     stat/init-age)))

(defn setup []
  (q/color-mode :hsb)
  (update-color-palette)
  ;; we need to "poke" react to redraw it after we got the right colors
  (swap! gui-ball-editor-state (comp vec reverse reverse))
  (q/frame-rate 15)
  (or @last-state (generate-new-state)))

(defn paint-over-with-squares [state]
  (doseq [ball (:old-balls state)]
    (draw/draw-squares state (sim/ball-intersecting ball))))

(defn- draw-squares-if-needed [state]
  (if-not @redraw-queued?
    ;; just draw what changed
    (do
      (when @draw-balls?
        (@paint-over-ball-shadows state))
      (draw/draw-squares state (map first (:changed state))))
    ;; draw every single square again
    (do (reset! redraw-queued? false)
        (draw/redraw-every-square state))))

(defn draw-state [state]
  (q/frame-rate @target-frame-rate)
  (when @every-second
    (draw/redraw-statistics state)
    (reset! current-frame-rate (int (q/current-frame-rate))))
  (draw-squares-if-needed state)
  (when @overlay
    (@overlay state)
    (reset! redraw-queued? true))
  (when @draw-balls?
    (draw/paint-balls state))
  (reset! every-second false))

(defn update-state [state]
  (let [state'
        (-> state
            update-simulation
            (assoc :old-squares (:squares state))
            stat/refresh-statistics)]
    (reset! last-state state')
    state'))


(defn run-sketch []
  (s/set-size!! @gui-size)
  (reset! redraw-queued? true)
  (reset! every-second true)
  (q/defsketch copied-squares
    :host "copied-squares"
    :size (state/get-size)
    :setup setup
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))

(defn controls []
  [:div.container.m-3
   [:div.row
    [:div.container.m-2.col-md-3.col-lg-2
     [:div.row [:h4 "simulation"]]
     [gui/checkbox ["balls collide with tiles" sim/collide-tiles? true]]
     [gui/checkbox ["balls paint tiles" sim/paint-tiles? true]]
     [gui/radio ["corner collisisions:" sim/point-collision :b
                 {:a ["reflect (preserves angle)" sim/collide-point-dumb]
                  :b ["fancy math thing" sim/collide-point-fancy]}]]
     [gui/int-slider ["steps per frame" state/ball-steps-per-frame 1 [1 500]]]]

    [:div.container.m-2.col-md-3.col-lg-2
     [:div.row [:h4 "visuals"]]
     [gui/checkbox ["draw balls?" draw-balls? true]]
     [gui/radio ["ball shadow paintover mode" paint-over-ball-shadows :b
                 {:a ["overlaping squares" paint-over-with-squares]
                  :b ["just ball" draw/paint-over-with-balls]
                  :c ["don't :3" identity]}]]
     [gui/button #(reset! redraw-queued? true) "redraw"]
     [gui/radio ["overlay" overlay :a
                 {:a ["none" nil]
                  :b ["clearlists" draw/draw-clearlists]
                  :c ["age" draw/draw-age]}]]]

    [:div.container.m-2.col-md-3.col-lg-2
     [:div.row [:h4 "other"]]
     [gui/int-slider ["frame rate" target-frame-rate 20 [1 60]]]
     [gui/int-slider ["drawing scale" (fn [newpx] (state/set-px!! newpx) (run-sketch)) 20 [1 60]]]
     [:div.row "current frame rate: " @current-frame-rate]]

    [:div.container.m-2.col-md-12.col-lg-5
     [:div.row [:h4 "initial conditions"] [:div.m-2 [gui/button (fn [] (reset! last-state nil) (run-sketch)) "restart"]]]
     [:div.row [gui/number-input "size (x):" (:x @gui-size) [0 100] int #(swap! gui-size assoc :x %)]]
     [:div.row [gui/number-input "size (y):" (:y @gui-size) [0 100] int #(swap! gui-size assoc :y %)]]
     [gui/radio ["initial square colors:" initial-squares :b
                 {:a ["closest ball" initial-squares-closest]
                  :b ["all gray" initial-squares-gray]}]]
     [gui/ball-edit-gui  gui-ball-editor-state random-ball]]]
   ;; TODO circular arrangement
   ])

(defn ^:export start []
  (rdom/render [controls] (js/document.getElementById "gui"))
  (js/setInterval #(reset! every-second true) 1000)
  (run-sketch))
