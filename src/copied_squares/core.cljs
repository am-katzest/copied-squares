(ns copied-squares.core
  (:require [quil.core :as q :include-macros true]
            [copied-squares.simulation :refer [update-simulation] :as sim]
            [copied-squares.gui :as gui]
            [copied-squares.drawing :as draw]
            [copied-squares.types :refer [make-xy xy* ->ball]]
            [copied-squares.state :refer [stat-size sizex sizey] :as state]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [quil.middleware :as m]))



(defn initial-squares []
  (vec (repeat (* sizey sizex) :gray)))

(defn rand-position [color vel size & {:keys [angle]}]
  (let [color (if (int? color) (keyword (str color)) color)
        position (make-xy (rand-int sizex) (rand-int sizey))
        angle (or angle (rand (* 2.0 Math/PI)))
        velocity (xy* (make-xy (Math/sin angle) (Math/cos angle)) vel)]
    (->ball color position velocity size)))

(defn setup []
  (q/frame-rate 15)
  (q/color-mode :hsb)
  (->
   {:frame 0
    :color-history []
    :squares (initial-squares)
    :clearlist-deltas {}
    :clearlist {}
    :balls (into [] (for [color [18 150 70 135 230]
                          angle [(/ Math/PI 4.05)]
                          _repetitions (range 1)]
                      (rand-position color 0.5 0.5 :angle angle)))}
   sim/create-clearlists))




(declare refresh-statistics)


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

(defn draw-state [state]
  (q/frame-rate @target-frame-rate)
  (when @every-second
    (reset! current-frame-rate (int (q/current-frame-rate))))
  (q/no-stroke)
  (if-not @redraw-queued?
    (do
      ;; just draw what changed
      (when @draw-balls?
        (@paint-over-ball-shadows state))
      (draw/draw-squares state (:changed state)))
    ;; draw every single square again
    (do (reset! redraw-queued? false)
        (->> (* sizex sizey)
             (range)
             (map state/inverse-coord)
             (draw/draw-squares state))
        ;; (doseq [i (range (* sizex sizey))
        ;;         :let [xy (sim/inverse-coord i)
        ;;               color (get-in state [:squares i])]]
        ;;   (draw-square (.-x xy) (.-y xy) color))
        ))

  (when @draw-clearlists?
    (draw/draw-clearlists state)
    (reset! redraw-queued? true))
  (when @draw-balls?
    (draw/paint-balls state))
  (draw/redraw-statistics state)
  (reset! every-second false))

(defn count-colors [state]
  (->> state
       :squares
       (reduce (fn [s x] (update s x #(inc (or % 0)))) {})
       (map (fn [[k v]] [k (/ v (* sizex sizey))]))
       (into {})))

(defn add-sliding [old new n]
  (take n (cons new old)))

(defn refresh-statistics [state]
  (-> state
      (update :color-history add-sliding (count-colors state) stat-size)))

(defn update-state [state]
  (-> state
      update-simulation
      refresh-statistics))

(defn run-sketch []
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
    [:div.row [:h4 "setup"]]
    [gui/button run-sketch "restart"]]
   ;; option for initial board
   ;; closest ball/all gray
   ;; would look neat with circular arrangement
   [:div.container.m-2
    [:div.row [:h4 "simulation"]]
    [gui/checkbox ["balls collide with tiles" sim/collide-tiles? true]]
    [gui/checkbox ["balls paint tiles" sim/paint-tiles? true]]
    [gui/radio ["corner collisisions:" sim/point-collision :a
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
    [:div.row "current frame rate: " @current-frame-rate]]])


(defn ^:export start []
  (rdom/render [controls] (js/document.getElementById "gui"))
  (js/setInterval #(reset! every-second true) 1000)
  (run-sketch))
