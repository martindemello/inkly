(ns inkling 
  (:import (javax.swing SwingUtilities JFrame JPanel WindowConstants)
           (javax.swing.event MouseInputListener)
           (java.awt Dimension Color Polygon)
           (java.awt.event MouseEvent)
           (java.awt.image BufferedImage)
           java.lang.Math))

(def +canvas-width+ 494)
(def +canvas-height+ 400)
(def +canvas-dimensions+ (new Dimension +canvas-width+ +canvas-height+))
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

(defn add-polygon! [model polygon]
  (let [g (model :g)
        bounds (.getBounds polygon)]
    (.setColor g Color/BLACK)
    (.fillPolygon g polygon)
    (doseq [f @(model :update-fns)] (f bounds))))

(defstruct <input-behavior> :on-mouse-pressed :on-mouse-released
                            :on-mouse-moved :on-mouse-dragged
                            :on-mouse-entered :on-mouse-exited
                            :on-mouse-clicked)

(defn do-nothing [behavior event] behavior)

(defn with-just-xy [f]
  (fn [behavior event] (f behavior (.getX event) (.getY event))))

(defn guard-button [button f]
  (fn [behavior event]
    (if (= (.getButton event) button)
        (f behavior event)
        behavior)))

(defn make-input-behavior []
  (struct-map <input-behavior> :on-mouse-pressed do-nothing
                               :on-mouse-released do-nothing
                               :on-mouse-moved do-nothing
                               :on-mouse-dragged do-nothing
                               :on-mouse-entered do-nothing
                               :on-mouse-exited do-nothing
                               :on-mouse-clicked do-nothing))

(defn make-input-dispatcher [event-name]
  (fn [behavior event] ((behavior event-name) behavior event)))

(def dispatch-mouse-pressed (make-input-dispatcher :on-mouse-pressed))
(def dispatch-mouse-released (make-input-dispatcher :on-mouse-released))
(def dispatch-mouse-moved (make-input-dispatcher :on-mouse-moved))
(def dispatch-mouse-dragged (make-input-dispatcher :on-mouse-dragged))
(def dispatch-mouse-entered (make-input-dispatcher :on-mouse-entered))
(def dispatch-mouse-exited (make-input-dispatcher :on-mouse-exited))
(def dispatch-mouse-clicked (make-input-dispatcher :on-mouse-clicked))

(defn make-input-handler [initial-behavior]
  (let [behavior (atom initial-behavior)]
    (proxy [MouseInputListener] []
      (mousePressed [e] (swap! behavior dispatch-mouse-pressed e))
      (mouseClicked [e] (swap! behavior dispatch-mouse-clicked e))
      (mouseReleased [e] (swap! behavior dispatch-mouse-released e))
      (mouseMoved [e] (swap! behavior dispatch-mouse-moved e))
      (mouseDragged [e] (swap! behavior dispatch-mouse-dragged e))
      (mouseEntered [e] (swap! behavior dispatch-mouse-entered e))
      (mouseExited [e] (swap! behavior dispatch-mouse-exited e)))))

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

(def make-draw-stroke-active-behavior) ;forward declaration

(defn make-draw-stroke-idle-behavior [model previous-pos]
  (let [on-mouse-pressed (fn [behavior x y]
                           (let [[previous-x previous-y] (get behavior :previous-pos [x y])
                                 builder (make-stroke-builder previous-x previous-y)
                                 builder (add-stroke-sample! model builder x y)]
                             (make-draw-stroke-active-behavior model builder)))
        on-mouse-pressed (with-just-xy on-mouse-pressed)
        on-mouse-pressed (guard-button MouseEvent/BUTTON1 on-mouse-pressed)

        on-mouse-moved (fn [behavior x y]
                         (assoc behavior :previous-pos [x y]))
        on-mouse-moved (with-just-xy on-mouse-moved)]

    (assoc (make-input-behavior) :on-mouse-pressed on-mouse-pressed
                                 :on-mouse-moved on-mouse-moved
                                 :on-mouse-dragged on-mouse-moved
                                 :previous-pos previous-pos)))

(defn make-draw-stroke-active-behavior [model builder]
  (let [on-mouse-released (fn [behavior x y]
                            (add-stroke-sample! model (behavior :builder) x y)
                            (make-draw-stroke-idle-behavior model [x y]))
        on-mouse-released (with-just-xy on-mouse-released)
        on-mouse-released (guard-button MouseEvent/BUTTON1 on-mouse-released)

        on-mouse-dragged (fn [behavior x y]
                            (let [builder (add-stroke-sample! model (behavior :builder) x y)]
                              (assoc behavior :builder builder)))
        on-mouse-dragged (with-just-xy on-mouse-dragged)]

    (assoc (make-input-behavior) :on-mouse-released on-mouse-released
                                 :on-mouse-dragged on-mouse-dragged
                                 :builder builder)))

(defn make-draw-stroke-behavior [model] (make-draw-stroke-idle-behavior model nil))

(defn make-canvas-component [model]
  (let [p (proxy [JPanel] []
            (paintComponent [g]
              (proxy-super paintComponent g)
              (render-model model g))
            (getMinimumSize [] +canvas-dimensions+)
            (getPreferredSize [] +canvas-dimensions+))
        listener (make-input-handler (make-draw-stroke-behavior model))]
    (.setBackground p Color/WHITE)
    (add-update-fn! model (fn [rect] (.repaint p (.x rect) (.y rect)
                                                 (.width rect)
                                                 (.height rect))))
    (.addMouseListener p listener)
    (.addMouseMotionListener p listener)
    p))

(defn make-toplevel-window []
  (let [w (new JFrame)
        model (make-model)
        v (make-canvas-component model)]
    (.setTitle w "Inkling")
    (.add w v)
    (.pack w)
    (.setBackground w Color/WHITE)
    (.setDefaultCloseOperation w WindowConstants/DISPOSE_ON_CLOSE)
    (.setResizable w false)
    (.setVisible w true)
    w))

(SwingUtilities/invokeAndWait make-toplevel-window)
