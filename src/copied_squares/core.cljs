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

(def gui-ball-editor-state (r/atom initial-balls))

(defn initial-squares-gray [_balls]
  (vec (repeat (* sizey sizex) :gray)))

(defn initial-squares-closest [balls]
  (vec (for [i (range (* sizey sizex))]
         (let [center (xy+ (state/inverse-coord i) (make-xy 0.5 0.5))]
           (:color (apply min-key #(xydist center (:position %)) balls))))))

(def initial-squares (atom initial-squares-gray))

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
(defn setup []
  (q/color-mode :hsb)
  (update-color-palette)
  ;; we need to "poke" react to redraw it after we got the right colors
  (swap! gui-ball-editor-state (comp vec reverse reverse))
  (q/frame-rate 15)
  (let [balls (into []
                    (for [{:keys [color count radius speed]} @gui-ball-editor-state
                          _count (range count)]
                      (rand-position color speed radius)))]
    (->
     {:frame 0
      :color-history []
      :squares (@initial-squares balls)
      :clearlist-deltas {}
      :clearlist {}
      :balls balls}
     sim/create-clearlists
     stat/count-colors)))

(defn paint-over-with-squares [state]
  (doseq [ball (:old-balls state)]
    (draw/draw-squares state (sim/ball-intersecting ball))))

(def paint-over-ball-shadows (atom paint-over-with-squares))
(def draw-balls? (atom true))
(def draw-clearlists? (atom true))
(def redraw-queued? (atom true))
(def target-frame-rate (atom 15))
(def current-frame-rate (r/atom 15))
(def every-second (atom true))

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
  (when @draw-clearlists?
    (draw/draw-clearlists state)
    (reset! redraw-queued? true))
  (when @draw-balls?
    (draw/paint-balls state))
  (reset! every-second false))

(defn update-state [state]
  (-> state
      update-simulation
      (assoc :old-squares (:squares state))
      stat/refresh-statistics))

(def gui-size (r/atom {:x 20 :y 20}))

(defn run-sketch []
  (s/set-size!! @gui-size)
  (reset! redraw-queued? true)
  (q/defsketch copied-squares
    :host "copied-squares"
    :size (state/get-size)
    :setup setup
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))

(defn controls []
  [:div.container.m-3
   [:div.container.m-2
    [:div.row [:h4 "simulation"]]
    [gui/checkbox ["balls collide with tiles" sim/collide-tiles? true]]
    [gui/checkbox ["balls paint tiles" sim/paint-tiles? true]]
    [gui/radio ["corner collisisions:" sim/point-collision :b
                {:a ["reflect (preserves angle)" sim/collide-point-dumb]
                 :b ["fancy math thing" sim/collide-point-fancy]}]]
    [gui/int-slider ["steps per frame" state/ball-steps-per-frame 1 [1 500]]]]

   [:div.container.m-2
    [:div.row [:h4 "visibility"]]
    [gui/checkbox ["draw balls?" draw-balls? true]]
    [gui/radio ["ball shadow paintover mode" paint-over-ball-shadows :b
                {:a ["overlaping squares" paint-over-with-squares]
                 :b ["just ball" draw/paint-over-with-balls]
                 :c ["don't :3" identity]}]]
    [gui/button #(reset! redraw-queued? true) "redraw"]
    [gui/checkbox ["draw clearlists?" draw-clearlists? false]]]

   [:div.container.m-2
    [:div.row [:h4 "other"]]
    [gui/int-slider ["frame rate" target-frame-rate 20 [1 60]]]
    [:div.row "current frame rate: " @current-frame-rate]]
   [:div.container.m-2
    [:div.row [:h4 "setup"]]
    [:div.row [gui/number-input "size (x):" (:x @gui-size) [0 100] int #(swap! gui-size assoc :x %)]]
    [:div.row [gui/number-input "size (y):" (:y @gui-size) [0 100] int #(swap! gui-size assoc :y %)]]
    [gui/radio ["initial square colors:" initial-squares :b
                {:a ["closest ball" initial-squares-closest]
                 :b ["all gray" initial-squares-gray]}]]
    [gui/ball-edit-gui  gui-ball-editor-state random-ball]
    [gui/button run-sketch "restart"]]
   ;; option for initial board
   ;; closest ball/all gray
   ;; would look neat with circular arrangement
   ])

(defn ^:export start []
  (rdom/render [controls] (js/document.getElementById "gui"))
  (js/setInterval #(reset! every-second true) 1000)
  (run-sketch)
  (swap! gui-ball-editor-state identity))
