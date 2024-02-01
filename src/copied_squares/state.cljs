(ns copied-squares.state
  (:require [copied-squares.types :refer [xy]]))
;┏━┓╻┏┳┓╻ ╻╻  ┏━┓╺┳╸╻┏━┓┏┓╻
;┗━┓┃┃┃┃┃ ┃┃  ┣━┫ ┃ ┃┃ ┃┃┗┫
;┗━┛╹╹ ╹┗━┛┗━╸╹ ╹ ╹ ╹┗━┛╹ ╹
(def sizex 25)
(def sizey 30)
(def size  (xy. sizex sizey))
(def delta_t 0.2)
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
(def stat-offset (* pxsq sizex))
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
