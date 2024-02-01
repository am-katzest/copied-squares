(ns copied-squares.core
  (:require [quil.core :as q :include-macros true]
            [copied-squares.simulation :refer [sizex sizey make-xy xy* ->ball update-simulation coord] :as sim]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [quil.middleware :as m]))

(def pxsq 25)
(defn px [x] (* x pxsq))

(defn initial-squares []
  (vec (repeat (* sizey sizex) :gray)))

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
    :balls (into [] (for [color [18 150 70]
                          angle [(/ Math/PI 4.05)]
                          _repetitions (range 1)]
                      (rand-position color 0.5 0.5 :angle angle)))}
   sim/create-clearlists))




(declare refresh-statistics)


(defn draw-square [x y color]
  (q/fill (wall-colors color))
  (q/rect (px x) (px y) pxsq pxsq))

(defn draw-small-square [x y color size]
  (q/fill color)
  (q/rect (+ size (px x)) (+ size (px y))
          (- pxsq size size) (- pxsq size size)))

(declare redraw-statistics)

(defn draw-squares [state squares]
  (doseq [xy squares
          :let [x (.-x xy)
                y (.-y xy)]]
    (draw-square x y (get-in state [:squares (coord xy)]))))


(defn paint-over-with-squares [state]
  (doseq [ball (:old-balls state)]
    (draw-squares state (sim/ball-intersecting ball))))

(def paint-over-ball-shadows (atom paint-over-with-squares))
(def draw-balls? (atom true))
(def draw-clearlists? (atom true))



(defn paint-ball [palette {:keys [color position radius]} & {:keys [scale] :or {scale 1}}]
  (let [xy position
        diam (* 2 scale (px radius))]
    (q/fill (palette color))
    (q/ellipse (px (.-x xy)) (px (.-y xy)) diam diam)))

(defn paint-over-with-balls [state]
  (doseq [ball (:old-balls state)]
    (paint-ball wall-colors ball :scale 1.05)))


(def redraw-queued? (atom true))
(def target-frame-rate (atom 15))
(def current-frame-rate (r/atom 15))
;; (add-watch target-frame-rate :update (fn [_ _ _ n] (q/frame-rate 30))) ; disallowed by quil
(def every-second (atom true))


(defn draw-clearlists [state]
  (doseq [[color deltas] (:clearlist-deltas state)
          :let [max (count deltas)
                scale (/ (* pxsq 0.5) max)]
          x (range sizex)
          y (range sizey)
          :let [val (get-in state [:clearlist color (coord x y)])]]
    (when (< val (/ max 2))
      (when (zero? (rand-int 100)) (println val))
      (draw-small-square x y [255 0 255 50] (+ (* pxsq 0.1) (* scale val)))))
  (reset! redraw-queued? true); it would ovrelap
  )

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
      (draw-squares state (:changed state)))
    ;; draw every single square again
    (do (reset! redraw-queued? false)
        (doseq [i (range (* sizex sizey))
                :let [xy (sim/inverse-coord i)
                      color (get-in state [:squares i])]]
          (draw-square (.-x xy) (.-y xy) color))
        ))
  (when @draw-clearlists?
    (draw-clearlists state))
  (when @draw-balls?
    (doseq [ball (:balls state)]
      (paint-ball ball-colors ball)))

  (redraw-statistics state)
  (reset! every-second false))

(defn count-colors [state]
  (->> state
       :squares
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

(defn run-sketch []
  (reset! redraw-queued? true)
  (q/defsketch copied-squares
    :host "copied-squares"
    :size [(+ (* stat-px stat-size) (px sizex)) (px sizey)]
    :setup setup
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))

(defn checkbox [[desc thing initial on off]]
  (let [state (r/atom initial)
        id (random-uuid)
        [on off] (if (some? on) [on off] [true false])
        sync-state #(reset! thing (if @state on off))]
    (sync-state)
    (fn [_]
      [:div.row
       [:div.form-check.p-1
        [:input.toggle
         {:id id
          :type "checkbox"
          :checked @state
          :on-change (fn []
                       (swap! state not)
                       (sync-state))}]
        [:label.form-check-label.pl-2 {:for id} desc]]])))

(defn radio [[desc thing initial states]]
  (let [state (r/atom initial)
        iid (random-uuid)]
    (fn [_]
      [:div.row [:div
                 desc
                 (let [chosen @state]
                   (for [[id [desc val]] states]
                     ^{:key id} [:div.form-check
                                 [:input.form-check-input
                                  {:type "radio"
                                   :name iid
                                   :id (str iid id)
                                   :value "option1"
                                   :checked (= id chosen)
                                   :on-change (fn []
                                                (reset! state id)
                                                (reset! thing val))}]
                                 [:label.form-check-label.pl-2 {:for (str iid id)} desc]]))]])))

(defn int-slider [[desc target initial [minimum maximum]]]
  (let [state (r/atom initial)
        check-bounds #(cond-> %
                        minimum (max minimum)
                        maximum (min maximum))
        maybe-swap! #(when-let [new-state (some-> % check-bounds int)]
                       (reset! target new-state)
                       (reset! state new-state))]
    (fn [_]
      [:div.row
       [:div.form-group.form-inline
        [:label.pr-2 {:for "textInput"} desc]
        [:input.form-control.px-2
         {:id "textInput"
          :type "text"
          :style {:width "5em" :height "1.5em"} ;; Custom width using inline style
          :value (str @state)
          :on-change #(maybe-swap! (-> % .-target .-value (js/parseInt)))}]
        [:input.form-control-range.mx-2
         {:type "range"
          :value @state
          :min (str minimum)
          :max (str maximum)
          :step 1
          :style {:width "100px"}
          :on-change #(maybe-swap! (-> % .-target .-value))}]]])))

(def dummy (atom nil))
(defn controls []
  [:div.container.m-3
   [:div.container.m-2
    [:div.row [:h4 "setup"]]
    [:button.btn.btn-primary {:type "button" :on-click run-sketch} "restart"]]

   [:div.container.m-2
    [:div.row [:h4 "simulation"]]
    [checkbox ["balls collide with tiles" sim/collide-tiles? true]]
    [checkbox ["balls paint tiles" sim/paint-tiles? true]]
    [radio ["corner collisisions:" dummy :a
            {:a ["reflect (preserves angle)" :todo]
             :b ["fancy math thing" :todo]}]]
    [int-slider ["steps per frame" sim/ball-steps-per-frame 1 [1 5000]]]]

   [:div.container.m-2
    [:div.row [:h4 "visibility"]]
    [checkbox ["draw balls?" draw-balls? true]]
    [radio ["ball shadow paintover mode" paint-over-ball-shadows :b
            {:a ["overlaping squares" paint-over-with-squares]
             :b ["just ball" paint-over-with-balls]
             :c ["don't :3" identity]}]]
    [:button.btn.btn-secondary {:type "button" :on-click #(reset! redraw-queued? true)} "redraw"]
    [checkbox ["draw clearlists?" draw-clearlists? false]]]
   [:div.container.m-2
    [:div.row [:h4 "other"]]
    [int-slider ["frame rate" target-frame-rate 20 [1 60]]]
    [:div.row "current frame rate: " @current-frame-rate]]])


(defn ^:export start []
  (rdom/render [controls] (js/document.getElementById "gui"))
  (js/setInterval #(reset! every-second true) 1000)
  (run-sketch))
