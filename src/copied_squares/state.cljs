(ns copied-squares.state
  (:require [copied-squares.types :refer [xy]]))
;┏━┓╻┏┳┓╻ ╻╻  ┏━┓╺┳╸╻┏━┓┏┓╻
;┗━┓┃┃┃┃┃ ┃┃  ┣━┫ ┃ ┃┃ ┃┃┗┫
;┗━┛╹╹ ╹┗━┛┗━╸╹ ╹ ╹ ╹┗━┛╹ ╹
(def sizex 25)
(def sizey 30)
(def size  (xy. sizex sizey))
(def delta_t 0.4)
(def ball-steps-per-frame (atom 1))

;╺┳┓┏━┓┏━┓╻ ╻╻┏┓╻┏━╸
; ┃┃┣┳┛┣━┫┃╻┃┃┃┗┫┃╺┓
;╺┻┛╹┗╸╹ ╹┗┻┛╹╹ ╹┗━┛

(def pxsq 25)
(defn px [x] (* x pxsq))

;┏━┓╺┳╸┏━┓╺┳╸╻┏━┓╺┳╸╻┏━╸┏━┓
;┗━┓ ┃ ┣━┫ ┃ ┃┗━┓ ┃ ┃┃  ┗━┓
;┗━┛ ╹ ╹ ╹ ╹ ╹┗━┛ ╹ ╹┗━╸┗━┛
(def stat-size 1000)
(def stat-every 10)
(def stat-px 5)
(def stat-offset (px sizex))
(def stat-width (* stat-size stat-px (/ stat-every)))
(def stat-height (px sizey))

;┏━┓╺┳╸┏━┓╺┳╸┏━╸┏━╸╻ ╻╻     ╻ ╻┏━╸╻  ┏━┓┏━╸┏━┓┏━┓
;┗━┓ ┃ ┣━┫ ┃ ┣╸ ┣╸ ┃ ┃┃     ┣━┫┣╸ ┃  ┣━┛┣╸ ┣┳┛┗━┓
;┗━┛ ╹ ╹ ╹ ╹ ┗━╸╹  ┗━┛┗━╸   ╹ ╹┗━╸┗━╸╹  ┗━╸╹┗╸┗━┛

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

(defn get-size [] [(+ (* stat-px stat-size) (px sizex)) (px sizey)])

;hacky, i'm unable to set this at start
(def rgb-colors (atom (into {}
                            (for [i (range 256)]
                              [(keyword (str i)) "#FFF"]))))

;; i understand your outrage...
;; but this function is only called when simulation isn't running
(defn set-size!! [{:keys [x y]}]
  (set! sizex x)
  (set! sizey y)
  (set! size (xy. sizex sizey))
  (set! stat-offset (px sizex))
  (set! stat-height (px sizey)))

(defn set-px!! [newpx]
  (set! pxsq newpx)
  (set! stat-offset (px sizex))
  (set! stat-height (px sizey))
  (set! stat-offset (px sizex))
  (set! stat-height (px sizey))
  )
