(ns copied-squares.core
  (:require [quil.core :as q :include-macros true]
            [copied-squares.simulation :refer [sizex sizey ->xy xy* ->ball update-simulation]]
            [reagent.dom :as rdom]
            [quil.middleware :as m]))

(def pxsq 25)
(defn px [x] (* x pxsq))


(def initial-squares
  (let [row-l (vec (concat (repeat (/ sizex 2) :gray)
                           (repeat (/ sizex 2) :gray)))]
    (vec (repeat sizey row-l))))

(let [make-pal #(into {} (map (fn [x] [(keyword (str x)) [x %1 %2]]) (range 256)))
      basic-wall (make-pal 180 200)
      basic-ball (make-pal 255 150)]
     (def wall-colors (merge basic-wall
                             {:gray [0 0 128]
                              :black [20 20 200]
                              :white  [0 0 20]}))
     (def ball-colors (merge basic-ball {:black [0 0 20]
                                         :white [20 20 200]})))




(defn rand-position [color vel size & {:keys [angle]}]
  (let [color (if (int? color) (keyword (str color)) color)
        position (->xy (rand-int sizex) (rand-int sizey))
        angle (or angle (rand (* 2.0 Math/PI)))
        velocity (xy* (->xy (Math/sin angle) (Math/cos angle)) vel)]
    (->ball color position velocity size)))

(defn setup []
  (q/frame-rate 60)
  (q/color-mode :hsb)
  {:frame 0
   :color-history []
   :squares initial-squares
   :balls (into [] (for [color [18 150 70]
                         angle [(/ Math/PI 4.05)]
                         _repetitions (range 1)]
                     (rand-position color 0.5 0.5 :angle angle)))})




(declare refresh-statistics)


(defn draw-square [x y color]
  (q/fill (wall-colors color))
  (q/rect (px x) (px y) pxsq pxsq))

(declare redraw-statistics)

(defn draw-state [state]
  ;; (q/background 240)
  (q/no-stroke)

  (if-let [changed (:changed state)]
    (do
      (doseq [{:keys [x y]} changed]
        (draw-square x y (get-in state [:squares x y])))
      (doseq [{:keys [x y]} (:redraw state)]
        (draw-square x y (get-in state [:squares x y]))))
    (doseq [[x row] (map-indexed vector (:squares state))
            [y color] (map-indexed vector row)]
      (draw-square x y color)))
  (doseq [{:keys [color position radius]} (:balls state)]
    (let [{:keys [x y]} position
          diam (* 2 (px radius))]
      (q/fill (ball-colors color))
      (q/ellipse (px x) (px y) diam diam)))
  (redraw-statistics state))

(defn count-colors [state]
  (->> state
       :squares
       (apply concat)
       (reduce (fn [s x] (update s x #(inc (or % 0)))) {})
       (map (fn [[k v]] [k (/ v (* sizex sizey))]))
       (into {})))

(defn add-sliding [old new n]
  (take n (cons new old)))
(def stat-size 100)
(def stat-px 5)
(def stat-offset (* pxsq sizex))
(def stat-width (* stat-size stat-px))
(def stat-height (px sizey))
(def history-every-frames 20)
(defn statistics-frame? [state] (zero? (mod (:frame state) history-every-frames)))

(defn refresh-statistics [state]
  (if (statistics-frame? state)
    (-> state
        (update :color-history add-sliding (count-colors state) stat-size))
    state))

(defn draw-color-history [state]
  (doseq [[i [prev next]] (map-indexed vector (partition 2 1 (:color-history state)))
          key (keys (into prev next))  ;unlikely, but data could be missing
          :let [prevv (get prev key 0)
                nextv (get next key 0)
                x1 (- (+ stat-width stat-offset) (* i stat-px))
                x2 (- (+ stat-width stat-offset) (* (inc i) stat-px))
                y1 (- stat-height (* stat-height prevv))
                y2 (- stat-height (* stat-height nextv))]]
    (apply q/stroke (wall-colors key))
    (q/stroke-weight 3)
    (q/line x1 y1 x2 y2)
    ))


(defn redraw-statistics [state]
  (when (statistics-frame? state)
    (q/fill 200)
    (q/rect stat-offset 0 stat-width stat-height)
    (draw-color-history state)))
(defn update-state [state]
  (-> state
      update-simulation
      refresh-statistics))


                                        ; this function is called in index.html
(defn run-sketch []
  (q/defsketch copied-squares
    :host "copied-squares"
    :size [(+ (* stat-px stat-size) (px sizex)) (px sizey)]
    :setup setup
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))


(defn controls []
  [:div
   [:button {:on-click run-sketch} "restart"]])


(defn ^:export start []
  (run-sketch)
  (rdom/render [controls] (js/document.getElementById "gui")))
