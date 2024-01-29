(ns copied-squares.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defrecord xy [x y])
(defrecord ball [color position velocity radius])

(def pxsq 25)
(defn px [x] (* x pxsq))
(def sizex 20)
(def sizey sizex)
(def size  (->xy sizex sizey))
(def initial-squares
  (let [row-l (vec (concat (repeat (/ sizex 2) :black)
                           (repeat (/ sizex 2) :white)))]
    (vec (repeat sizey row-l))))

(def wall-colors {:black [20 20 200]
                  :white  [0 0 20]})
(def ball-colors {:black [0 0 20]
                  :white [20 20 200]})

(def delta_t 0.5)
(def physic_frames_per_refresh 5)

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  {:squares initial-squares
   :balls [(->ball :white (->xy 10 15) (->xy 0.1 0.1) 4)
           (->ball :black (->xy 12 5) (->xy 0.5 0.6) .5)
           (->ball :black (->xy 12 3) (->xy 0.6 0.5) .5)
           (->ball :black (->xy 12 7) (->xy 0.5 0.7) .5)
           (->ball :black (->xy 10 7) (->xy 0.5 0.71) .5)
           (->ball :black (->xy 10 5) (->xy 0.63 0.39) .5)
           (->ball :black (->xy 10 3) (->xy 0.7 0.5) .5)
           ]})
(defn low [x] (if (pos? x) x (- x)))
(defn high [x] (if (neg? x) x (- x)))

(defn xy+ [a b] (->xy (+ (:x a) (:x b)) (+ (:y a) (:y b))))
(defn xy- [a b] (->xy (- (:x a) (:x b)) (- (:y a) (:y b))))
(defn xy* [a f] (->xy (* (:x a) f) (* (:y a) f)))

(defn xydist [a b]
  (let [{:keys [x y]} (xy- a b)]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn apply-vel [ball]
  (update ball :position xy+ (xy* (:velocity ball) delta_t)))

(defn collide-in-past [ball coord wall side]
  (let [ball' (update-in ball [:velocity coord] side)]
    ;; TODO
    ;; reflect velocity and move it as if it collided in the past
    ball'))
(defn collide-point [ball point]
  (let [ball' (update-in ball [:velocity] #(xy- (->xy 0 0) %))]
    ;; TODO
    ;; reflect velocity and move it as if it collided in the past
    ball'))

(defn collide-walls [coord {:keys [position radius] :as ball}]
  (let [posx (coord position)]
    (cond (> (+ posx radius) (coord size)) (collide-in-past ball coord nil high)
          (< (- posx radius) 0) (collide-in-past ball coord nil low)
          :else ball)))

(defn move-ball [ball]
  (let [ball' (->> ball apply-vel (collide-walls :x) (collide-walls :y))]
    ball'))
(defn inside-board? [{:keys [x y]}]
  (and (< -1 x sizex)
       (< -1 y sizey)))
(defn between [upper-bound lower-bound x]
  (max (min x upper-bound) lower-bound))

(defn closest-point [{:keys [x y]} ball]
  (->xy (between (inc x) x (:x (:position ball)))
        (between (inc y) y (:y (:position ball)))))


(defn inside? [point {:keys [position radius]}]
  (> radius (xydist position point)))
;; we are travelling the ball this way ->
;; 546
;; 213
;; 879
;; first upwards, than downwards, spreading left and right until we find a tile not intersecting with ball

(def south (->xy 0 1))
(def north (->xy 0 -1))
(def west (->xy -1 0))
(def east (->xy 1 0))

(defn append-while "for horizontal flood" [acc condition next element]
  (if-not (condition element) acc
          (recur (conj acc element) condition next (xy+ next element))))

(defn append-while-splitting "for vertical flood"
  [acc condition element next]
  (if-not (condition element) acc
          (recur (-> acc
                  (conj element)
                  (append-while condition west (xy+ west element))
                  (append-while condition east (xy+ east element)))
                 condition (xy+ next element) next)))

(defn ball-intersecting [ball squares]
  (let [{:keys [x y]} (:position ball)
        good? #(and (inside-board? %) (inside? (closest-point % ball) ball))
        init (->xy (Math/floor x) (Math/floor y))]
    (-> []
        (append-while-splitting good? init north)
        (append-while-splitting good? (xy+ south init) south))))


(defn update-state-ball [state {:keys [color] :as ball}]
  (let [ball' (move-ball ball)

        squares (:squares state)
        intersecting (->> state
                          :squares
                          (ball-intersecting ball')
                          (filter (fn [{:keys [x y]}] (not= (get-in squares [x y]) color))))]
    (if (empty? intersecting) (update state :balls conj ball')
        (let [chosen (apply min-key #(xydist (closest-point % ball) (:position ball)) intersecting)
              {:keys [x y] :as point} (closest-point chosen ball)
              ball'' (case [(int? x) (int? y)]
                       [true true]  (collide-point ball point) ; corner
                       [false false] ball' ; center (uh oh)
                       [true false] (collide-in-past ball' :x x (if (pos? (:x (:velocity ball))) high low)) ; vertical
                       [false true] (collide-in-past ball' :y y (if (pos? (:y (:velocity ball))) high low)) ; horizontal
                       )]
          (println "ball " color " collided with " chosen)
          (-> state
              (update :balls conj ball'')
              (assoc-in [:squares (:x chosen) (:y chosen)] color)
              ;;TODO add diff
              )))))

(defn update-state-once [state]
  (reduce update-state-ball (dissoc state :balls) (:balls state)))

(defn update-state [state]
  (nth (iterate update-state-once (dissoc state :change)) physic_frames_per_refresh))

(defn draw-state [state]
  ;; (q/background 240)
  (q/no-stroke)
  (doseq [[x row] (map-indexed vector (:squares state))
          [y color] (map-indexed vector row)]
    (q/fill (wall-colors color))
    (q/rect (px x) (px y) pxsq pxsq))
  (doseq [{:keys [color position radius]} (:balls state)]
    (let [{:keys [x y]} position
          diam (* 2 (px radius))]
      (q/fill (ball-colors color))
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
