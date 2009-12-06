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
                                         make-input-listener]]
        [org.inkscape.inkly.syntax :only [infix-math]])
  (:gen-class))

(def +canvas-width+ 494)
(def +canvas-height+ 400)
(def +canvas-dimensions+ (new Dimension +canvas-width+ +canvas-height+))
(def +canvas-rect+ (new Rectangle +canvas-dimensions+))
(def +pen-width+ (double 30))
(def +half-pen-width+ (/ +pen-width+ 2.0))
(def +motion-epsilon+ (double 2.0))

(defstruct <buffer> :image :g)

(defn make-buffer []
  (let [image (new BufferedImage +canvas-width+ +canvas-height+
                   BufferedImage/TYPE_INT_ARGB_PRE)
        g (.createGraphics image)]
    (struct-map <buffer> :image image :g g)))

(defstruct <model> :canvas-buffer :overlay-buffer :update-fns)

(declare clear-canvas!)
(declare clear-overlay!)

(defn make-model []
  (let [canvas-buffer (make-buffer)
        overlay-buffer (make-buffer)
        model (struct-map <model> :canvas-buffer canvas-buffer
                                  :overlay-buffer overlay-buffer
                                  :update-fns (atom ()))]
    (clear-canvas! model)
    (clear-overlay! model)
    model))

(defn invoke-update-fns [model rect]
  (doseq [f @(model :update-fns)] (f rect)))

(defn render-model [model g]
  (.setComposite g AlphaComposite/Src)
  (.drawImage g ((model :canvas-buffer) :image) 0 0 nil)
  (.setComposite g AlphaComposite/SrcOver)
  (.drawImage g ((model :overlay-buffer) :image) 0 0 nil))

(defn clear-canvas! [model]
  (let [g ((model :canvas-buffer) :g)]
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
  (let [g ((model :overlay-buffer) :g)]
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
        g ((model :overlay-buffer) :g)
        bounds (.getBounds poly)]
    (.setColor g Color/BLACK)
    (.fill g poly)
    (invoke-update-fns model bounds)))

(defn draw-canvas-polygon! [model points]
  (let [poly (make-polygon points)
        g ((model :canvas-buffer) :g)
        bounds (.getBounds poly)]
    (.setColor g Color/BLACK)
    (.fill g poly)
    (invoke-update-fns model bounds)))

(defn vadd [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn vsub [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(defn vmag [[x y]]
  (infix-math (Math/sqrt ((x * x) + (y * y)))))

(defn vscale [s [x y]] [(* s x) (* s y)])

(defn vscaleinv [s [x y]] [(/ x s) (/ y s)])

(defn vnorm [v]
  (let [m (vmag v)]
    (if (= m 0.0)
      [(double 0.0) (double 0.0)]
      (vscaleinv m v))))

; anti-clockwise in screen coordinate system
(defn rot90 [[x y]] [(- y) x])

(defn segments-intersect? [[[x0 y0] [x1 y1]] [[x2 y2] [x3 y3]]]
  (let [x0 (double x0) y0 (double y0)
        x1 (double x1) y1 (double y1)
        x2 (double x2) y2 (double y2)
        x3 (double x3) y3 (double y3)
        dx0 (- x1 x0) dx1 (- x3 x2)
        dy0 (- y1 y0) dy1 (- y3 y2)
        denom (- (* dy1 dx0) (* dx1 dy0))
        relx (- x0 x2) rely (- y0 y2)
        num0 (- (* dx1 rely) (* dy1 relx))
        num1 (- (* dx0 rely) (* dy0 relx))]
    (if (= denom 0.0)
        ; parallel or coincident
        false
        ; else, intersecting lines
        (let [t0 (/ num0 denom) t1 (/ num1 denom)]
          ; check if intersections lie within segments
          (and (and (>= t0 0.0) (<= t0 1.0))
               (and (>= t1 0.0) (<= t1 1.0)))))))

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
              (when (segments-intersect? [p0 p3] [p1 p2])
                (println (.concat "Side bowtie: " (str [[p0 p3] [p1 p2]]))))
              (when (segments-intersect? [p0 p1] [p2 p3])
                (println (.concat "Longitudinal bowtie: " (str [[p0 p1] [p2 p3]]))))
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
