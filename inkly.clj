; inkly - a little drawing app of modest aspirations
;
; Copyright (c) 2009 MenTaLguY <mental@rydia.net>
;
; Permission is hereby granted, free of charge, to any person obtaining
; a copy of this software and associated documentation files (the
; "Software"), to deal in the Software without restriction, including
; without limitation the rights to use, copy, modify, merge, publish,
; distribute, sublicense, and/or sell copies of the Software, and to
; permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
;
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
(ns inkly 
  (:import [javax.swing SwingUtilities JFrame JPanel WindowConstants]
           [java.awt Dimension Color Polygon Rectangle]
           [java.awt.event MouseEvent KeyEvent]
           [java.awt.image BufferedImage]
           java.lang.Math)
  (:use [org.inkscape.inkly.input :only [make-input-behavior
                                         compose-input-behaviors
                                         make-input-listener]])
  (:gen-class))

(def +canvas-width+ 494)
(def +canvas-height+ 400)
(def +canvas-dimensions+ (new Dimension +canvas-width+ +canvas-height+))
(def +canvas-rect+ (new Rectangle +canvas-dimensions+))
(def +pen-width+ (double 30))
(def +half-pen-width+ (/ +pen-width+ 2.0))
(def +motion-epsilon+ (double 3.0))

(defstruct <model> :image :g :update-fns)

(defn make-model []
  (let [image (new BufferedImage +canvas-width+ +canvas-height+
                   BufferedImage/TYPE_INT_ARGB_PRE)
        g (.createGraphics image)
        model (struct-map <model> :image image :g g :update-fns (atom ()))]
    (.setColor g Color/WHITE)
    (.fillRect g 0 0 +canvas-width+ +canvas-height+)
    model))

(defn render-model [model g]
  (.drawImage g (model :image) 0 0 nil))

(defn add-update-fn! [model callback]
  (swap! (model :update-fns) conj callback))

(defn invoke-update-fns [model rect]
  (doseq [f @(model :update-fns)] (f rect)))

(defn add-polygon! [model polygon]
  (let [g (model :g)
        bounds (.getBounds polygon)]
    (.setColor g Color/BLACK)
    (.fillPolygon g polygon)
    (invoke-update-fns model bounds)))

(defn clear-canvas! [model]
  (let [g (model :g)]
    (.setColor g Color/WHITE)
    (.fillRect g 0 0 +canvas-width+ +canvas-height+)
    (invoke-update-fns model +canvas-rect+)))

(defstruct <stroke-builder> :previous-x :previous-y :previous-sides)

(defn make-stroke-builder [previous-x previous-y]
  (struct-map <stroke-builder> :previous-x (double previous-x)
                               :previous-y (double previous-y)
                               :previous-sides nil))

(defn add-stroke-sample! [model builder x y]
  (let [x (double x)
        y (double y)
        dx (- x (builder :previous-x))
        dy (- y (builder :previous-y))
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (if (< dist +motion-epsilon+)
        builder
        (let [angle-x (- (/ dy dist))
              angle-y (/ dx dist)
              offset-x (* angle-x +half-pen-width+)
              offset-y (* angle-y +half-pen-width+)
              x2 (- x offset-x)
              y2 (- y offset-y)
              x3 (+ x offset-x)
              y3 (+ y offset-y)
              previous-sides (builder :previous-sides)
              builder (struct-map <stroke-builder>
                                  :previous-x x :previous-y y
                                  :previous-sides [[x3 y3] [x2 y2]])]
          (when (not (nil? previous-sides))
            (let [[[x0 y0] [x1 y1]] previous-sides
                  poly (new Polygon)]
              (.addPoint poly x0 y0)
              (.addPoint poly x1 y1)
              (.addPoint poly x2 y2)
              (.addPoint poly x3 y3)
              (add-polygon! model poly)))
          builder))))

(defn with-just-xy [f]
  (fn [behavior event] (f behavior (.getX event) (.getY event))))

(defn guard-button [button f]
  (fn [behavior event]
    (if (= (.getButton event) button)
        (f behavior event)
        behavior)))

(declare make-draw-stroke-active-behavior)

(defn make-draw-stroke-idle-behavior [model previous-pos]
  (let [on-mouse-pressed
          (guard-button MouseEvent/BUTTON1 (with-just-xy
            (fn [behavior x y]
              (let [[previous-x previous-y] (get behavior :previous-pos [x y])
                    builder (make-stroke-builder previous-x previous-y)
                    builder (add-stroke-sample! model builder x y)]
                (make-draw-stroke-active-behavior model builder)))))

        on-mouse-moved
          (with-just-xy
            (fn [behavior x y] (assoc behavior :previous-pos [x y])))]

    (make-input-behavior :on-mouse-pressed on-mouse-pressed
                         :on-mouse-moved on-mouse-moved
                         :on-mouse-dragged on-mouse-moved
                         :previous-pos previous-pos)))

(defn make-draw-stroke-active-behavior [model builder]
  (let [on-mouse-released
          (guard-button MouseEvent/BUTTON1 (with-just-xy
            (fn [behavior x y]
              (add-stroke-sample! model (behavior :builder) x y)
              (make-draw-stroke-idle-behavior model [x y]))))

        on-mouse-dragged
          (with-just-xy
            (fn [behavior x y]
              (let [builder (add-stroke-sample! model (behavior :builder) x y)]
                (assoc behavior :builder builder))))]

    (make-input-behavior :on-mouse-released on-mouse-released
                         :on-mouse-dragged on-mouse-dragged
                         :builder builder)))

(defn make-draw-stroke-behavior [model] (make-draw-stroke-idle-behavior model nil))

(defn make-clear-canvas-behavior [model]
  (let [on-key-pressed
          (fn [behavior event]
            (when (= (.getKeyCode event) KeyEvent/VK_ESCAPE)
              (clear-canvas! model))
            behavior)]
    (make-input-behavior :on-key-pressed on-key-pressed)))

(defn make-canvas-component [model]
  (let [p (proxy [JPanel] []
            (paintComponent [g]
              (proxy-super paintComponent g)
              (render-model model g))
            (getMinimumSize [] +canvas-dimensions+)
            (getPreferredSize [] +canvas-dimensions+))
        behaviors (compose-input-behaviors (make-clear-canvas-behavior model)
                                           (make-draw-stroke-behavior model))
        listener (make-input-listener behaviors)]
    (.setBackground p Color/WHITE)
    (add-update-fn! model #(.repaint p (.x %) (.y %) (.width %) (.height %)))
    (.addMouseListener p listener)
    (.addMouseMotionListener p listener)
    (.addKeyListener p listener)
    (.setFocusable p true)
    p))

(defn make-toplevel-window []
  (let [w (new JFrame)
        model (make-model)
        v (make-canvas-component model)]
    (.setTitle w "Inkly")
    (.add w v)
    (.pack w)
    (.setBackground w Color/WHITE)
    (.setDefaultCloseOperation w WindowConstants/DISPOSE_ON_CLOSE)
    (.setResizable w false)
    (.setVisible w true)
    w))

(defn -main [& args] ())

(defn main [& args]
  (SwingUtilities/invokeAndWait make-toplevel-window))

(when (not *compile-files*)
  (apply main *command-line-args*))
