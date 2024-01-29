(ns copied-squares.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def pxsq 25)
(defn px [x] (* x pxsq))
(def sizex 20)
(def sizey sizex)
(def initial-squares
  (let [row-l (vec (concat (repeat (/ sizex 2) :black)
                           (repeat (/ sizex 2) :white)))]
    (repeat sizey row-l)))
(def colors {:black [0 0 20]
             :white [20 20 200]})

(def delta_t 0.1)
(def physic_frames_per_refresh 2)

(defrecord xy [x y])
(defrecord ball [color position velocity radius])
(defn setup []
                                        ; Set frame rate to 30 frames per second.
  (q/frame-rate 60)
                                        ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
                                        ; setup function returns initial state. It contains
                                        ; circle color and position.
  {:squares initial-squares
   :balls [(->ball :white (->xy 10 5) (->xy 0.5 0.5) .5)
           (->ball :black (->xy 10 15) (->xy 0.5 0.5) .5)]})
(defn absp [x] (if (pos? x) x (- x)))
(defn absn [x] (if (neg? x) x (- x)))

(defn xy+ [a b] (->xy (+ (:x a) (:x b)) (+ (:y a) (:y b))))
(defn xy* [a f] (->xy (* (:x a) f) (* (:y a) f)))

(defn apply-vel [ball]
  (update ball :position xy+ (xy* (:velocity ball) delta_t)))

(defn collide-wallsx [{:keys [position radius] :as ball}]
  (let [posx (:x position)]
    (cond (> (+ posx radius) sizex) (update-in ball [:velocity :x] absn)
          (< (- posx radius) 0) (update-in ball [:velocity :x] absp)
          :else ball)))

(defn collide-wallsy [{:keys [position radius] :as ball}]
  (let [posy (:y position)]
    (cond (> (+ posy radius) sizey) (update-in ball [:velocity :y] absn)
          (< (- posy radius) 0) (update-in ball [:velocity :y] absp)
          :else ball)))

(defn move-ball [ball squares]
  (let [ball' (-> ball apply-vel collide-wallsx collide-wallsy)]
    ball'))

(defn update-state-ball [state ball]
  (update state :balls conj (move-ball ball (:squares state))))

(defn update-state [state]
  (reduce update-state-ball (dissoc state :balls) (:balls state)))

(defn draw-state [state]
  ;; (q/background 240)
  (q/no-stroke)
  (doseq [[x row] (map-indexed vector (:squares state))
          [y color] (map-indexed vector row)]
    (q/fill (colors color))
    (q/rect (px x) (px y) pxsq pxsq))
  (doseq [{:keys [color position radius]} (:balls state)]
    (let [{:keys [x y]} position
          diam (* 2 (px radius))]
      (q/fill (colors color))
      (q/ellipse (px x) (px y) diam diam))))

; this function is called in index.html
(defn ^:export run-sketch []
  (q/defsketch copied-squares
    :host "copied-squares"
    :size [(* pxsq sizex) (* pxsq sizey)]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))

; uncomment this line to reset the sketch:
; (run-sketch)
