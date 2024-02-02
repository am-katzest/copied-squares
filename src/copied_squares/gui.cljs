(ns copied-squares.gui
  (:require [reagent.core :as r]
            [copied-squares.state :as s]))


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
    (reset! thing (second (initial states)))
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
    (maybe-swap! initial)
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


(defn number-input [description value [minimum maximum] conversion on-call]
  (let [check-bounds #(cond-> %
                        minimum (max minimum)
                        maximum (min maximum))]
    [:div.form-group.form-inline
     [:label.px-2 {:for "textInput"} description]
     [:input.form-control.px-2
      {:id "textInput"
       :type "text"
       :style {:width "5em" :height "1.5em"} ;; Custom width using inline style
       :value value
       :on-change #(some-> % .-target .-value js/parseFloat conversion check-bounds on-call)}]]))

(defn color-slider [color-id on-call]
  [:input.form-control-range.mx-2
   {:type "range"
    :value (name color-id)
    :min "0"
    :max "255"
    :step 1
    :style {:width "100px" :accent-color "#555"} ;styling them is hell
    :on-change #(-> % .-target .-value keyword on-call)}
   ])

(defn button [f text]
  [:button.btn.btn-secondary {:type "button" :on-click f} text])

(defn ball-edit-list [state-atom]
  [:div.list-group
   (for [[i {:keys [color count radius speed]}] (map-indexed vector @state-atom)]
     (do (println (str "1px solid " (@s/rgb-colors color)))
         ^{:key i} [:a.list-group-item.my-1.p-0
                    [:div.form-group.form-inline.m-0.px-0
                     {:style {:border (str "4px solid " (@s/rgb-colors color))}}
                     [number-input "count:" count [0 10] int #(swap! state-atom assoc-in [i :count] %)]
                     [number-input "radius:" radius [0 10] float #(swap! state-atom assoc-in [i :radius] %)]
                     [number-input "speed:" speed [0 10] float #(swap! state-atom assoc-in [i :speed] %)]
                     [color-slider color #(swap! state-atom assoc-in [i :color] %)]
                     [:button.btn
                      {:type "button"
                       :style {:font-size "10px"}
                       ;; TODO fix removing
                       :on-click #(swap! state-atom (fn [x] (filterv some? (assoc x i nil))))}
                      "⌫"]]]))])
(defn ball-edit-gui [state-atom new-fn]
  [:div.container
   [ball-edit-list  state-atom]
   [button #(swap! state-atom conj (new-fn)) "add new"]])
