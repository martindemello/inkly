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
           [java.awt Dimension Color Rectangle AlphaComposite
                     RenderingHints]
           [java.awt.geom Path2D Path2D$Float]
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
(def +motion-epsilon+ (double 2.0))

(defstruct <model> :canvas-image :overlay-image :canvas-g :overlay-g :update-fns)

(declare clear-canvas!)
(declare clear-overlay!)

(defn make-model []
  (let [make-image (fn [] (new BufferedImage +canvas-width+ +canvas-height+
                               BufferedImage/TYPE_INT_ARGB_PRE))
        canvas-image (make-image)
        overlay-image (make-image)
        canvas-g (.createGraphics canvas-image)
        overlay-g (.createGraphics overlay-image)
        model (struct-map <model> :canvas-image canvas-image
                                  :canvas-g canvas-g
                                  :overlay-image overlay-image
                                  :overlay-g overlay-g
                                  :update-fns (atom ()))]
    (clear-canvas! model)
    (clear-overlay! model)
    model))

(defn invoke-update-fns [model rect]
  (doseq [f @(model :update-fns)] (f rect)))

(defn render-model [model g]
  (.setComposite g AlphaComposite/Src)
  (.drawImage g (model :canvas-image) 0 0 nil)
  (.setComposite g AlphaComposite/SrcOver)
  (.drawImage g (model :overlay-image) 0 0 nil))

(defn clear-canvas! [model]
  (let [g (model :canvas-g)]
    (.setColor g Color/WHITE)
    (.setComposite g AlphaComposite/Src)
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING
                         RenderingHints/VALUE_ANTIALIAS_OFF)
    (.fillRect g 0 0 +canvas-width+ +canvas-height+)
    (.setComposite g AlphaComposite/SrcOver)
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING
                         RenderingHints/VALUE_ANTIALIAS_ON)
    (invoke-update-fns model +canvas-rect+)))

(defn clear-overlay! [model]
  (let [g (model :overlay-g)]
    (.setComposite g AlphaComposite/Clear)
    (.fillRect g 0 0 +canvas-width+ +canvas-height+)
    (.setComposite g AlphaComposite/SrcOver)
    (invoke-update-fns model +canvas-rect+)))

(defn add-update-fn! [model callback]
  (swap! (model :update-fns) conj callback))

(defn make-polygon [points]
  (let [path (new Path2D$Float)]
    (.setWindingRule path Path2D/WIND_NON_ZERO)
    (when (not (empty? points))
      (let [[initial-point & remaining-points] points
            move-to (fn [[x y]] (.moveTo path x y))
            line-to (fn [[x y]] (.lineTo path x y))]
        (move-to initial-point)
        (dorun (map line-to remaining-points)))
      (.closePath path))
    path))

(defn draw-overlay-quad! [model p0 p1 p2 p3]
  (let [poly (make-polygon [p0 p1 p2 p3])
        g (model :overlay-g)
        bounds (.getBounds poly)]
    (.setColor g Color/BLACK)
    (.fill g poly)
    (invoke-update-fns model bounds)))

(defn draw-canvas-polygon! [model points]
  (let [poly (make-polygon points)
        g (model :canvas-g)
        bounds (.getBounds poly)]
    (.setColor g Color/BLACK)
    (.fill g poly)
    (invoke-update-fns model bounds)))

(defn vadd [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn vsub [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(defn vmag [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn vscale [s [x y]] [(* s x) (* s y)])

(defn vscaleinv [s [x y]] [(/ x s) (/ y s)])

(defn vnorm [v]
  (let [m (vmag v)]
    (if (= m 0.0)
      [(double 0.0) (double 0.0)]
      (vscaleinv m v))))

; anti-clockwise in screen coordinate system
(defn rot90 [[x y]] [(- y) x])

(defn stroke-points [pos vin vout]
  (let [stroke-angle (rot90 (vnorm (vadd vin vout)))
        stroke-offset (vscale +half-pen-width+ stroke-angle)]
    [(vsub pos stroke-offset) (vadd pos stroke-offset)]))

(defstruct <stroke-builder> :previous-pos :previous-vel :stroke-sides)

(defn make-stroke-builder [x y]
  (struct-map <stroke-builder> :previous-pos [x y]
                               :previous-vel [(double 0.0) (double 0.0)]
                               :stroke-sides []))

(defn add-stroke-sample! [model builder x y]
  (let [pos [(double x) (double y)]
        old-pos (builder :previous-pos)
        vel (vsub pos old-pos)
        mvel (vmag vel)]
    (if (< mvel +motion-epsilon+)
        builder
        ; else
        (let [old-vel (builder :previous-vel)
              [p2 p3] (stroke-points old-pos old-vel vel)
              stroke-sides (builder :stroke-sides)
              builder (struct-map <stroke-builder>
                                  :previous-pos pos
                                  :previous-vel vel
                                  ; note order: [p3 p2] become [p0 p1]
                                  :stroke-sides (cons [p3 p2] stroke-sides))]
          (when (not (empty? stroke-sides))
            (let [[p0 p1] (first stroke-sides)]
              (draw-overlay-quad! model p0 p1 p2 p3)))
          builder))))

(defn complete-stroke! [model builder]
  (let [old-vel (builder :previous-vel)
        old-pos (builder :previous-pos)
        [p2 p3] (stroke-points old-pos old-vel old-vel)
        stroke-sides (builder :stroke-sides)]
    (when (not (empty? stroke-sides))
      (clear-overlay! model)
      (let [points (concat (map first stroke-sides)
                           (map second (reverse stroke-sides)))]
        (draw-canvas-polygon! model points)))))

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
                    builder (make-stroke-builder x y)
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
              (let [builder (add-stroke-sample! model (behavior :builder) x y)]
                (complete-stroke! model builder)
                (make-draw-stroke-idle-behavior model [x y])))))

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
    (.setTitle w "~Inkly~")
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
