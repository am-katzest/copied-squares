(ns copied-squares.drawing
  (:require [quil.core :as q]
            [copied-squares.state :refer [px pxsq sizex sizey coord] :as s]))

(let [make-pal #(into {} (map (fn [x] [(keyword (str x)) [x %1 %2]]) (range 256)))
      basic-wall (make-pal 180 200)
      basic-ball (make-pal 255 150)]
  (def wall-colors (merge basic-wall
                          {:gray [0 0 128]
                           :black [20 20 200]
                           :white  [0 0 20]}))
  (def ball-colors (merge basic-ball {:black [0 0 20]
                                      :white [20 20 200]})))

(defn draw-square [x y color]
  (q/fill (wall-colors color))
  (q/rect (px x) (px y) pxsq pxsq))

(defn draw-small-square [x y color size]
  (q/fill color)
  (q/rect (+ size (px x)) (+ size (px y))
          (- pxsq size size) (- pxsq size size)))

(defn draw-squares [state squares]
  (doseq [xy squares
          :let [x (.-x xy)
                y (.-y xy)]]
    (draw-square x y (get-in state [:squares (coord xy)]))))

(defn paint-ball [palette {:keys [color position radius]} & {:keys [scale] :or {scale 1}}]
  (let [xy position
        diam (* 2 scale (px radius))]
    (q/fill (palette color))
    (q/ellipse (px (.-x xy)) (px (.-y xy)) diam diam)))

(defn paint-over-with-balls [state]
  (q/no-stroke)
  (doseq [ball (:old-balls state)]
    (paint-ball wall-colors ball :scale 1.05)))

(defn paint-balls [state]
  (q/no-stroke)
  (doseq [ball (:balls state)]
    (paint-ball ball-colors ball)))

(defn draw-clearlists [state]
  (q/no-stroke)
  (doseq [[color deltas] (:clearlist-deltas state)
          :let [max (count deltas)
                scale (/ (* pxsq 0.5) max)]
          x (range sizex)
          y (range sizey)
          :let [val (get-in state [:clearlist color (coord x y)])]]
    (when (< val (/ max 2))
      (draw-small-square x y [255 0 255 (if (zero? val) 70 40)] (+ (* pxsq 0.1) (* scale val))))))



(defn draw-color-history [state]
  (doseq [[i [prev next]] (map-indexed vector (partition 2 1 (:color-history state)))
          key (keys (into prev next))   ;unlikely, but data could be missing
          :let [prevv (get prev key 0)
                nextv (get next key 0)
                x1 (+ (+ s/stat-offset) (* i s/stat-px))
                x2 (+ (+ s/stat-offset) (* (inc i) s/stat-px))
                y1 (- s/stat-height (* s/stat-height (/ prevv sizex sizey)))
                y2 (- s/stat-height (* s/stat-height (/ nextv sizex sizey)))]]
    (apply q/stroke (wall-colors key))
    (q/stroke-weight 3)
    (q/line x1 y1 x2 y2)))

(defn redraw-statistics [state]
  (q/no-stroke)
  (q/fill 200)
  (q/rect s/stat-offset 0 s/stat-width s/stat-height)
  (draw-color-history state))

(defn redraw-every-square [state]
  (q/no-stroke)
  (->> (* sizex sizey)
       (range)
       (map s/inverse-coord)
       (draw-squares state)))
