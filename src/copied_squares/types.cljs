(ns copied-squares.types)

(deftype xy [x y])

(defn make-xy [x y] (xy. x y))

(defrecord ball [color position velocity radius])

(defn xy+ [a b] (xy. (+ (.-x a) (.-x b)) (+ (.-y a) (.-y b))))

(defn xy-
  ([a b] (xy. (- (.-x a) (.-x b)) (- (.-y a) (.-y b))))
  ([a] (xy. (- (.-x a)) (- (.-y a)))))

(defn xy* [a f] (xy. (* (.-x a) f) (* (.-y a) f)))

(defn xymag [xy]
  (let [x (.-x xy)
        y (.-y xy)]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn xydist [a b]
  (xymag (xy- a b)))

(defn xydot [a b]
  (+ (* (.-x a) (.-x b))
     (* (.-y a) (.-y b))))

(defn update-xy [point axis f]
  (case axis
    :y (xy. (.-x point) (f (.-y point)))
    :x (xy. (f (.-x point)) (.-y point))))
