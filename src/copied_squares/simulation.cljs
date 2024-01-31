(ns copied-squares.simulation)

(defrecord xy [x y])
(defrecord ball [color position velocity radius])

(defn xy+ [a b] (->xy (+ (:x a) (:x b)) (+ (:y a) (:y b))))
(defn xy- [a b] (->xy (- (:x a) (:x b)) (- (:y a) (:y b))))
(defn xy* [a f] (->xy (* (:x a) f) (* (:y a) f)))

(def sizex 15)

(def sizey sizex)
(def size  (->xy sizex sizey))


(def delta_t 0.25)
(def physic_frames_per_refresh 4)

(defn low [x] (if (pos? x) x (- x)))
(defn high [x] (if (neg? x) x (- x)))


(defn xydist [a b]
  (let [{:keys [x y]} (xy- a b)]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn apply-vel [ball]
  (update ball :position xy+ (xy* (:velocity ball) delta_t)))

(defn collide-in-past [ball coord wall side]
  (let [ball' (update-in ball [:velocity coord] side)]
    ;; TODO move it as if it collided in the past
    ball'))
(defn collide-point [ball point]
  (let [ball' (update-in ball [:velocity] #(xy- (->xy 0 0) %))]
    ;; TODO  collide properly and move it as if it collided in the past
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

(defn ball-intersecting [ball]
  (let [{:keys [x y]} (:position ball)
        good? #(and (inside-board? %) (inside? (closest-point % ball) ball))
        init (->xy (Math/floor x) (Math/floor y))]
    (-> []
        (append-while-splitting good? init north)
        (append-while-splitting good? (xy+ south init) south))))


(defn update-state-ball [state {:keys [color] :as ball}]
  (let [ball' (move-ball ball)

        squares (:squares state)
        intersecting (->> (ball-intersecting ball')
                          (filter (fn [{:keys [x y]}] (not= (get-in squares [x y]) color))))]
    (if (empty? intersecting) (update state :balls conj ball')
        (let [chosen (apply min-key #(xydist (closest-point % ball) (:position ball)) intersecting)
              {:keys [x y] :as point} (closest-point chosen ball)
              ball'' (case [(int? x) (int? y)]
                       [true true]  (collide-point ball point) ; corner
                       [false false] ball' ; center (uh oh)
                       [true false] (collide-in-past ball' :x x (if (pos? (:x (:velocity ball'))) high low)) ; vertical
                       [false true] (collide-in-past ball' :y y (if (pos? (:y (:velocity ball'))) high low)) ; horizontal
                       )]
          (loop [state (update state :balls conj ball'')
                 [first & rest] intersecting]
            (if first
              (-> state
                  (assoc-in [:squares (:x chosen) (:y chosen)] color)
                  (update :changed conj chosen)
                  (recur rest))
              state)
              )))))

(defn update-state-once [state]
  (reduce update-state-ball (dissoc state :balls) (:balls state)))

(defn update-simulation [state]
  (-> (iterate update-state-once (assoc state :changed [] :redraw (mapcat ball-intersecting (:balls state))))
      (nth physic_frames_per_refresh)
      (update :frame inc)))
