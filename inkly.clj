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
  (:import [javax.swing SwingUtilities JFrame JPanel WindowConstants
                        JToolBar JToggleButton ButtonGroup Icon JButton
                        JSlider JMenuBar JMenu JMenuItem BorderFactory]
           [javax.swing.event ChangeListener]
           [java.awt Dimension Color Rectangle AlphaComposite
                     RenderingHints BorderLayout Insets Polygon]
           [java.awt.geom Area]
           [java.awt.event MouseEvent KeyEvent ActionListener]
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
(def +default-pen-width+ (double 30))
(def +min-pen-width+ (double 1))
(def +max-pen-width+ (double 50))
(def +motion-epsilon+ (double 2.0))
(def +color-icon-size+ 16)

(defstruct <buffer> :image :g)

(defn make-buffer []
  (let [image (new BufferedImage +canvas-width+ +canvas-height+
                   BufferedImage/TYPE_INT_ARGB_PRE)
        g (.createGraphics image)]
    (struct-map <buffer> :image image :g g)))

(defstruct <model> :current-color
                   :pen-width
                   :blobs
                   :overlay-buffer
                   :update-fns)

(declare clear-canvas!)
(declare clear-overlay!)

(defn make-model []
  (let [overlay-buffer (make-buffer)
        model (struct-map <model> :current-color (atom Color/BLACK)
                                  :pen-width (atom +default-pen-width+)
                                  :blobs (atom [])
                                  :overlay-buffer overlay-buffer
                                  :update-fns (atom ()))]
    (clear-canvas! model)
    (clear-overlay! model)
    model))

(defn invoke-update-fns [model rect]
  (doseq [f @(model :update-fns)] (f rect)))

(defn render-model [model g]
  (.setComposite g AlphaComposite/Src)
  (.setColor g Color/WHITE)
  (.fillRect g 0 0 +canvas-width+ +canvas-height+)
  (.setComposite g AlphaComposite/SrcOver)
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
  (let [render-blob (fn [[color area]]
                      (.setColor g color)
                      (.fill g area))]
    (dorun (map render-blob @(model :blobs))))
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_OFF)
  (.drawImage g ((model :overlay-buffer) :image) 0 0 nil))

(defn clear-canvas! [model]
  (reset! (model :blobs) [])
  (invoke-update-fns model +canvas-rect+))

(defn clear-overlay! [model]
  (let [g ((model :overlay-buffer) :g)]
    (.setComposite g AlphaComposite/Clear)
    (.setColor g Color/WHITE)
    (.fillRect g 0 0 +canvas-width+ +canvas-height+)
    (.setComposite g AlphaComposite/SrcOver)
    (invoke-update-fns model +canvas-rect+)))

(defn set-current-color! [model color]
  (reset! (model :current-color) color)
  nil)

(defn set-pen-width! [model width]
  (reset! (model :pen-width) width)
  nil)

(defn add-update-fn! [model callback]
  (swap! (model :update-fns) conj callback))

(defn draw-overlay-shape! [model color shape]
  (let [g ((model :overlay-buffer) :g)
        bounds (.getBounds shape)]
    (.setColor g color)
    ; compensate for antialiasing weirdness
    (when (not= color Color/WHITE)
      (.translate g -0.5 -0.5))
    (.fill g shape)
    (when (not= color Color/WHITE)
      (.translate g 0.5 0.5))
    (.grow bounds 1 1)
    (invoke-update-fns model bounds)))

(defn draw-canvas-shape! [model color shape]
  (let [bounds (.getBounds shape)]
    (swap! (model :blobs) conj [color shape])
    (invoke-update-fns model bounds)))

(defn erase-canvas-shape! [model shape]
  (let [bounds (.getBounds shape)
        erase-in-one (fn [[color shape2]]
                       (let [shape2 (doto (.clone shape2)
                                      (.subtract shape))]
                         [color shape2]))
        erase-in (fn [blobs]
                   (apply vector
                     (for [b (map erase-in-one blobs)
                           :when (not (.isEmpty (second b)))] b)))]
    (swap! (model :blobs) erase-in)
    (.grow bounds 1 1)
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

(defn stroke-points [pos width vin vout]
  (let [stroke-angle (rot90 (vnorm (vadd vin vout)))
        stroke-offset (vscale (/ width 2.0) stroke-angle)]
    [(vsub pos stroke-offset) (vadd pos stroke-offset)]))

(defstruct <stroke-builder> :color
                            :width
                            :previous-pos
                            :previous-vel
                            :stroke-sides)

(defn make-stroke-builder [color width x y]
  (struct-map <stroke-builder> :color color
                               :width width
                               :previous-pos [x y]
                               :previous-vel [(double 0.0) (double 0.0)]
                               :stroke-sides []))

(defn add-stroke-sample! [model area builder x y]
  (let [pos [(double x) (double y)]
        old-pos (builder :previous-pos)
        vel (vsub pos old-pos)
        mvel (vmag vel)]
    (if (< mvel +motion-epsilon+)
        builder
        ; else
        (let [width (builder :width)
              old-vel (builder :previous-vel)
              [p2 p3] (stroke-points old-pos width old-vel vel)
              stroke-sides (builder :stroke-sides)
              builder (assoc builder :previous-pos pos
                                     :previous-vel vel
                                     ; note order: [p3 p2] become [p0 p1]
                                     :stroke-sides (cons [p3 p2]
                                                         stroke-sides))]
          (when (not (empty? stroke-sides))
            (let [[p0 p1] (first stroke-sides)
                  quad (new Polygon)]
              (dorun (map (fn [[x y]] (.addPoint quad x y)) [p0 p1 p2 p3]))
              (.add area (new Area quad))
              (let [color (builder :color)
                    color (if (= color :erase) Color/WHITE color)]
                (draw-overlay-shape! model color quad))))
          builder))))

(defn complete-stroke! [model area builder]
  (let [width (builder :width)
        old-vel (builder :previous-vel)
        old-pos (builder :previous-pos)
        [p2 p3] (stroke-points old-pos width old-vel old-vel)
        stroke-sides (builder :stroke-sides)]
    (when (not (empty? stroke-sides))
      (clear-overlay! model)
      (let [points (concat (map first stroke-sides)
                           (map second (reverse stroke-sides)))
            color (builder :color)]
        (if (= color :erase)
          (erase-canvas-shape! model area)
          (draw-canvas-shape! model color area))))))

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
                    color @(model :current-color)
                    width @(model :pen-width)
                    area (new Area)
                    builder (make-stroke-builder color width x y)
                    builder (add-stroke-sample! model area builder x y)]
                (make-draw-stroke-active-behavior model area builder)))))

        on-mouse-moved
          (with-just-xy
            (fn [behavior x y] (assoc behavior :previous-pos [x y])))]

    (make-input-behavior :on-mouse-pressed on-mouse-pressed
                         :on-mouse-moved on-mouse-moved
                         :on-mouse-dragged on-mouse-moved
                         :previous-pos previous-pos)))

(defn make-draw-stroke-active-behavior [model area builder]
  (let [on-mouse-released
          (guard-button MouseEvent/BUTTON1 (with-just-xy
            (fn [behavior x y]
              (let [builder (add-stroke-sample! model area (behavior :builder) x y)]
                (complete-stroke! model area builder)
                (make-draw-stroke-idle-behavior model [x y])))))

        on-mouse-dragged
          (with-just-xy
            (fn [behavior x y]
              (let [builder (add-stroke-sample! model area (behavior :builder) x y)]
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

(defn make-color-icon [color]
  (proxy [Icon] []
    (getIconHeight [] +color-icon-size+)
    (getIconWidth [] +color-icon-size+)
    (paintIcon [c g x y]
      (if (= color :erase)
        (do (.setColor g Color/WHITE)
            (.fillRect g x y +color-icon-size+ +color-icon-size+)
            (.setColor g Color/RED)
            (let [opp-x (- (+ x +color-icon-size+) 1)
                  opp-y (- (+ y +color-icon-size+) 1)]
              (.drawLine g x y opp-x opp-y)
              (.drawLine g x opp-y opp-x y)))
        (do (.setColor g color)
            (.fillRect g x y +color-icon-size+ +color-icon-size+))))))

(defn make-toolbar-color-button [model group color]
  (let [button (new JToggleButton)
        listener (proxy [ActionListener] []
                   (actionPerformed [e]
                     (set-current-color! model color)))]
    (.add group button)
    (.setMargin button (new Insets 0 0 0 0))
    (.setIcon button (make-color-icon color))
    (.addActionListener (.getModel button) listener)
    button))

(defn make-toolbar-clear-button [model]
  (let [button (new JButton "clear")
        listener (proxy [ActionListener] []
                   (actionPerformed [e]
                     (clear-canvas! model)))]
    (.setMargin button (new Insets 1 1 1 1))
    (.addActionListener button listener)
    button))

(defn make-toolbar-width-slider [model]
  (let [slider (new JSlider JSlider/HORIZONTAL
                            +min-pen-width+
                            +max-pen-width+
                            +default-pen-width+)
        range-model (.getModel slider)
        listener (proxy [ChangeListener] []
                   (stateChanged [e]
                     (when (not (.getValueIsAdjusting range-model))
                       (let [width (.getValue range-model)]
                         (set-pen-width! model width)))))]
    (.addChangeListener range-model listener)
    (.setOpaque slider false)
    slider))

(defn make-toolbar-component [model]
  (let [toolbar (new JToolBar)
        color-button-group (new ButtonGroup)
        make-color-button #(make-toolbar-color-button model color-button-group %)
        erase-button (make-color-button :erase) 
        black-button (make-color-button Color/BLACK)
        clear-all-button (make-toolbar-clear-button model)
        pen-width-slider (make-toolbar-width-slider model)]
    (.setBorder toolbar (BorderFactory/createLoweredBevelBorder))
    (.setFloatable toolbar false)
    (.setRollover toolbar true)
    (.add toolbar black-button)
    (.add toolbar erase-button)
    (.setSelected (.getModel black-button) true)
    (.addSeparator toolbar)
    (.add toolbar clear-all-button)
    (.addSeparator toolbar)
    (.add toolbar pen-width-slider)
    toolbar))

(defn make-menubar [model]
  (let [menubar (new JMenuBar)
        file-menu (new JMenu "File")
        export-svg-item (new JMenuItem "Export as SVG...")
        close-item (new JMenuItem "Close")]
    (.add file-menu export-svg-item)
    (.addSeparator file-menu)
    (.add file-menu close-item)
    (.add menubar file-menu)
    menubar))

(defn make-toplevel-window []
  (let [w (new JFrame)
        model (make-model)
        menubar (make-menubar model)
        canvas (make-canvas-component model)
        toolbar (make-toolbar-component model)]
    (.setTitle w "~Inkly~")
    (.setLayout w (new BorderLayout))
    (.setJMenuBar w menubar)
    (.add w toolbar BorderLayout/SOUTH)
    (.add w canvas BorderLayout/CENTER)
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
