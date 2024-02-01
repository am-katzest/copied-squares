(ns copied-squares.simulation)

(deftype xy [x y])
(defn make-xy [x y] (xy. x y))

(defrecord ball [color position velocity radius])

(defn xy+ [a b] (xy. (+ (.-x a) (.-x b)) (+ (.-y a) (.-y b))))
(defn xy- [a b] (xy. (- (.-x a) (.-x b)) (- (.-y a) (.-y b))))
(defn xy* [a f] (xy. (* (.-x a) f) (* (.-y a) f)))
(defn xydist [a b]
  (let [xy (xy- a b)
        x (.-x xy)
        y (.-y xy)]
    (Math/sqrt (+ (* x x) (* y y)))))
(defn xydot [a b]
  (+ (* (.-x a) (.-x b))
     (* (.-y a) (.-y b))))

(defn update-xy [point axis f]
  (case axis
    :y (xy. (.-x point) (f (.-y point)))
    :x (xy. (f (.-x point)) (.-y point))))

(def sizex 25)

(def sizey sizex)
(def size  (xy. sizex sizey))
(defn coord
  ([point]
   (+ (.-x point)
      (* sizex (.-y point))))
  ([x y] (+ x (* sizex y))))
(defn closest-coord [point]
  (+ (int (.-x point))
     (* sizex (int (.-y point)))))
(defn inverse-coord [coord-value]
  (let [y (quot coord-value sizex)
        x (- coord-value (* sizex y))]
    (xy. x y)))

(def delta_t 0.5)
(def ball-steps-per-frame (atom 1))

(defn low [x] (if (pos? x) x (- x)))
(defn high [x] (if (neg? x) x (- x)))

(defn apply-vel [ball]
  (update ball :position xy+ (xy* (:velocity ball) delta_t)))

(defn collide-in-past [ball coord wall side]
  (let [ball' (update ball :velocity update-xy coord side)]
    ;; TODO move it as if it collided in the past
    ball'))

(defn collide-point [ball point]
  (if (neg? (xydot (:velocity ball) (- (:position ball) point)))
    ball                                ; already moving away
    (let [ball' (update-in ball [:velocity] #(->xy (- (.-y %)) (- (.-x %))))]
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

(def paint-tiles? (atom true))
(def collide-tiles? (atom true))
(declare update-clearlists)

(defn- paint-tiles [state' intersecting color]
  (loop [state state' [first & rest] intersecting]
    (if-not first state
      (let [oldcolor (get-in state [:squares (coord first)])]
        (-> state
            (assoc-in [:squares (coord first)] color)
            (update :changed conj first)
            (update-clearlists oldcolor color first)
            (recur rest))))))

(defn- collide-ball [point ball]
  (let [x (.-x point)
        y (.-y point)]
    (case [(int? x) (int? y)]
      [true true]  (collide-point ball point)       ; corner
      [false false] ball                           ; center (uh oh)
      [true false] (collide-in-past ball :x x (if (pos? (.-x (:velocity ball))) high low)) ; vertical
      [false true] (collide-in-past ball :y y (if (pos? (.-y (:velocity ball))) high low)) ; horizontal
      )))

;; program keeps "clearlists", arrays of indexes where any ball has no chance of collision

(defn update-clearlist [list origin deltas change]
  (loop [[delta & rest] deltas
         list list]
    (if (nil? delta) list
        (let [xy' (xy+ origin delta)]
          (recur rest (cond-> list
                        (inside-board? xy') (update  (coord xy') change)))))))

(defn update-clearlists [state oldcolor newcolor xy]
  ;; (println oldcolor newcolor (count (get-in state [:clearlist-deltas newcolor])))
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
                    (map coord)
                    sort
                    dedupe
                    (mapv inverse-coord))
        clearlist (vec (repeat (* sizex sizey) (- (count deltas) 2)))]
    (-> state
        (assoc-in [:clearlist-deltas color] deltas)
        (assoc-in [:clearlist color] clearlist))))

(defn create-clearlists [state]
  (->> state
       :balls
       (map (fn [b] [(:color b) (:radius b)]))
       ;; (filter (fn [[_ radius]] (pos? radius))) no need to keep them for points
       sort
       dedupe
       (reduce add-clearlist state)))

(defn in-clearlist? [state {:keys [color position]}]
  ;; (println color (get-in state [:clearlist color (closest-coord position)]))
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
  (reduce update-state-ball (dissoc state :balls) (:balls state)))

(defn update-simulation [state]
  (-> (iterate update-state-once (assoc state :changed [] :old-balls (:balls state)))
      (nth @ball-steps-per-frame)
      (update :frame inc)))
