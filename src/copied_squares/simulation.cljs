(ns copied-squares.simulation
  (:require
   [copied-squares.state :refer [sizex sizey size delta_t coord inverse-coord closest-coord ball-steps-per-frame]]
   [copied-squares.types :refer [xy update-xy ->xy xy+ xy* xydot xymag xy- map->ball xydist]]
   [quil.core :refer [acos atan2]]))



(defn low [x] (if (pos? x) x (- x)))

(defn high [x] (if (neg? x) x (- x)))

(defn apply-vel [ball]
  (update ball :position xy+ (xy* (:velocity ball) delta_t)))

(defn collide-in-past [ball coord _wall side]
  (let [ball' (update ball :velocity update-xy coord side)]
    ;; TODO move it as if it collided in the past
    ball'))

(defn collide-point-dumb [ball _point]
  (update-in ball [:velocity] #(->xy (- (.-y %)) (- (.-x %)))))


(defn to-the-right? [a b]
  ;; https://stackoverflow.com/questions/13221873/determining-if-one-2d-vector-is-to-the-right-or-left-of-another
  (let [angleacw (+ (/ Math/PI 2) (atan2 (.-y a) (.-x a)))
        arot (xy. (Math/cos angleacw) (Math/sin angleacw))]
    (pos? (xydot arot b))))

(defn calc-angle [a b]
  ((if (to-the-right? a b) + -) (acos (/ (xydot a b) (xymag a) (xymag b)))))

(defn collide-point-fancy [{:keys [position velocity] :as ball} point]
  ;; https://physics.stackexchange.com/questions/464343/elastic-collision-between-a-circle-and-a-point
  (let [angle (atan2 (.-y velocity) (.-x velocity))
        a velocity
        b (xy- point position)
        relative-angle (calc-angle a b)
        angle' (+ Math/PI angle (* 2 relative-angle))
        speed (xymag velocity)          ; will drift
        x (* speed (Math/cos angle'))
        y (* speed (Math/sin angle'))
        xy (xy. x y)]
    (if (or (js/isNaN x) (js/isNaN y)) ball
        (assoc ball :velocity xy))))

(def point-collision (atom nil))

(defn collide-point [ball point]
  (if (neg? (xydot (:velocity ball) (- (:position ball) point)))
    ball                                ; already moving away
    (let [ball' (@point-collision ball point)]
      ;; TODO  collide properly and move it as if it collided in the past
      ball')))

(defn collide-walls-x [{:keys [position radius] :as ball}]
  (let [pos (.-x position)]
    (cond (> (+ pos radius) (.-x size)) (collide-in-past ball :x nil high)
          (< (- pos radius) 0) (collide-in-past ball :x nil low)
          :else ball)))

(defn collide-walls-y [{:keys [position radius] :as ball}]
  (let [pos (.-y position)]
    (cond (> (+ pos radius) (.-y size)) (collide-in-past ball :y nil high)
          (< (- pos radius) 0) (collide-in-past ball :y nil low)
          :else ball)))

(defn move-ball [ball]
  (let [ball' (->> ball apply-vel collide-walls-x collide-walls-y)]
    ball'))

(defn inside-board? [point]
  (and (< -1 (.-x point) sizex)
       (< -1 (.-y point) sizey)))

(defn between [upper-bound lower-bound x]
  (max (min x upper-bound) lower-bound))

(defn closest-point [point ball]
  (let [x (.-x point)
        y (.-y point)]
    (xy. (between (inc x) x (.-x (:position ball)))
         (between (inc y) y (.-y (:position ball))))))

(defn inside? [point {:keys [position radius]}]
  (> radius (xydist position point)))
;; we are travelling the ball this way ->
;; 546
;; 213
;; 879
;; first upwards, than downwards, spreading left and right until we find a tile not intersecting with ball

(def south (xy. 0 1))
(def north (xy. 0 -1))
(def west (xy. -1 0))
(def east (xy. 1 0))

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

(defn ball-intersecting
  ([ball ignore-walls]
   (let [center (:position ball)
         good? #(and (or (inside-board? %) ignore-walls) (inside? (closest-point % ball) ball))
         init (xy. (Math/floor (.-x center)) (Math/floor (.-y center)))]
     (-> []
         (append-while-splitting good? init north)
         (append-while-splitting good? (xy+ south init) south))))
  ([ball] (ball-intersecting ball false)))

(def paint-tiles? (atom nil))

(def collide-tiles? (atom nil))

(declare update-clearlists)

(defn- paint-tiles [state' intersecting color]
  (loop [state state' [first & rest] intersecting]
    (if-not first state
      (let [oldcolor (get-in state [:squares (coord first)])]
        (-> state
            (assoc-in [:squares (coord first)] color)
            (update :changed conj [first oldcolor color])
            (update-clearlists oldcolor color first)
            (recur rest))))))

(defn- collide-ball [point ball]
  (if (> (rand) 0.99) ball
    (let [x (.-x point)
          y (.-y point)]
      (case [(int? x) (int? y)]
        [true true]  (collide-point ball point) ; corner
        [false false] ball                      ; center (uh oh)
        [true false] (collide-in-past ball :x x (if (pos? (.-x (:velocity ball))) high low)) ; vertical
        [false true] (collide-in-past ball :y y (if (pos? (.-y (:velocity ball))) high low)) ; horizontal
        ))))

;; program keeps "clearlists", arrays of indexes where any ball has no chance of collision

(defn update-clearlist [list origin deltas change]
  (loop [[delta & rest] deltas
         list list]
    (if (nil? delta) list
        (let [xy' (xy+ origin delta)]
          (recur rest (cond-> list
                        (inside-board? xy') (update  (coord xy') change)))))))

(defn update-clearlists [state oldcolor newcolor xy]
  (let [newdeltas (get-in state [:clearlist-deltas newcolor])
        olddeltas (get-in state [:clearlist-deltas oldcolor])]
    (cond-> state
     newdeltas (update-in [:clearlist newcolor] update-clearlist xy newdeltas dec)
     olddeltas (update-in [:clearlist oldcolor] update-clearlist xy olddeltas inc))))

;; only runs once, can be slow
(defn add-clearlist [state [color radius]]
  ;; deltas are relative positions of squares (relative to the one containing ball)
  ;; with which ball could collide
  ;; value for each square contains the number of collidable neighbours
  (let [deltas (->> (for [x [0.01 0.5 0.99]
                          y [0.01 0.5 0.99]
                          :let [xy (xy. x y)
                                ball (map->ball {:position xy :radius radius})]
                          b (ball-intersecting ball true)]
                      b)
                    (map (fn [xy] [(.-x xy) (.-y xy)]))
                    sort
                    dedupe
                    (mapv (fn  [[x y]] (xy. x y))))
        clearlist (vec (repeat (* sizex sizey) (count deltas)))]
    (-> state
        (assoc-in [:clearlist-deltas color] deltas)
        (assoc-in [:clearlist color] clearlist))))

(defn initialize-clearlists [state]
  (->> state
       :squares
       (map-indexed vector)
       (reduce (fn [state [i color]]
                 (update-clearlists state :gray color (inverse-coord i)))
          state)))

(defn create-clearlists [state]
  (->> state
       :balls
       (map (fn [b] [(:color b) (:radius b)]))
       sort
       dedupe
       (reduce add-clearlist state)
       initialize-clearlists))

(defn in-clearlist? [state {:keys [color position]}]
  (zero? (get-in state [:clearlist color (closest-coord position)])))

(defn update-state-ball [state {:keys [color] :as ball}]
  (if (in-clearlist? state ball)
    (update state :balls conj (apply-vel ball))
    (let [ball' (move-ball ball)

          squares (:squares state)
          intersecting (->> (ball-intersecting ball')
                            (filter (fn [xy] (not= (get squares (coord xy)) color))))]
      (if (empty? intersecting) (update state :balls conj ball')
          (let [chosen (apply min-key #(xydist (closest-point % ball) (:position ball)) intersecting)
                point  (closest-point chosen ball)
                ball'' (if-not @collide-tiles? ball' (collide-ball point ball'))
                state' (update state :balls conj ball'')]
            (if-not @paint-tiles? state'
                    (paint-tiles state' intersecting color)))))))

(defn update-state-once [state]
  (-> (reduce update-state-ball (dissoc state :balls) (:balls state))
      (update :frame inc)))

(defn update-simulation [state]
  (-> (iterate update-state-once (assoc state :changed [] :old-balls (:balls state)))
      (nth @ball-steps-per-frame)))
