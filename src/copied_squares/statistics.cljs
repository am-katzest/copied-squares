(ns copied-squares.statistics
  (:require [copied-squares.state :as s]))

(defn count-colors [state]
  (assoc state :color-history
   (->> state
        :squares
        (reduce (fn [s x] (update s x #(inc (or % 0)))) {})
        (map (fn [[k v]] [k v]))
        (into {})
        (vector))))

(defn update-colors [previous state]
  (loop [colors previous
         [[change old new] & rest] (:changed state)]
    (if-not change colors
            (-> colors
                (update new #(inc (or % 0)))
                (update old #(dec %))
                (recur rest)))))

(defn add-next
  "takes turns [a b c d] into [b c d (f d state)]"
  [list state f]
  ;; todo use subvec?
  (let [new-element (f (last list) state)
        list' (conj list new-element)
        cnt (count list')
        drop? (>= cnt s/stat-size)]
    (subvec list' (if drop? 1 0) cnt)))

(defn refresh-statistics [state]
  (-> state
      (update :color-history add-next state update-colors)))
