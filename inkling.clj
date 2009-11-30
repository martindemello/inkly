(ns inkling 
  (:import (javax.swing SwingUtilities JFrame JPanel WindowConstants)
           (javax.swing.event MouseInputListener)
           (java.awt Dimension Color Polygon)
           (java.awt.image BufferedImage)))

(def +canvas-width+ 494)
(def +canvas-height+ 400)
(def +canvas-dimensions+ (new Dimension +canvas-width+ +canvas-height+))

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

(defn make-input-behavior []
  (let [do-nothing (fn [behavior event] behavior)]
    (struct-map <input-behavior> :on-mouse-pressed do-nothing
                                 :on-mouse-released do-nothing
                                 :on-mouse-moved do-nothing
                                 :on-mouse-dragged do-nothing
                                 :on-mouse-entered do-nothing
                                 :on-mouse-exited do-nothing
                                 :on-mouse-clicked do-nothing)))

(defn make-draw-dots-behavior [model]
  (let [draw-dot-at-mouse (fn [behavior event]
                            (let [x (.getX event) y (.getY event)
                                  poly (new Polygon)]
                              (.addPoint poly (- x 2) (- y 2))
                              (.addPoint poly (- x 2) (+ y 2))
                              (.addPoint poly (+ x 2) (+ y 2))
                              (.addPoint poly (+ x 2) (- y 2))
                              (add-polygon! model poly))
                            behavior)]
    (assoc (make-input-behavior) :on-mouse-pressed draw-dot-at-mouse
                                 :on-mouse-dragged draw-dot-at-mouse)))

(defn make-input-dispatcher [event-name]
  (fn [behavior event] ((behavior event-name) behavior event)))

(def dispatch-mouse-pressed (make-input-dispatcher :on-mouse-pressed))
(def dispatch-mouse-released (make-input-dispatcher :on-mouse-released))
(def dispatch-mouse-moved (make-input-dispatcher :on-mouse-moved))
(def dispatch-mouse-dragged (make-input-dispatcher :on-mouse-dragged))
(def dispatch-mouse-entered (make-input-dispatcher :on-mouse-entered))
(def dispatch-mouse-exited (make-input-dispatcher :on-mouse-exited))
(def dispatch-mouse-clicked (make-input-dispatcher :on-mouse-clicked))

(defn make-input-handler [behavior]
  (let [behavior (atom behavior)]
    (proxy [MouseInputListener] []
      (mousePressed [e] (swap! behavior dispatch-mouse-pressed e))
      (mouseClicked [e] (swap! behavior dispatch-mouse-clicked e))
      (mouseReleased [e] (swap! behavior dispatch-mouse-released e))
      (mouseMoved [e] (swap! behavior dispatch-mouse-moved e))
      (mouseDragged [e] (swap! behavior dispatch-mouse-dragged e))
      (mouseEntered [e] (swap! behavior dispatch-mouse-entered e))
      (mouseExited [e] (swap! behavior dispatch-mouse-exited e)))))

(defn make-canvas-component [model]
  (let [p (proxy [JPanel] []
            (paintComponent [g]
              (proxy-super paintComponent g)
              (render-model model g))
            (getMinimumSize [] +canvas-dimensions+)
            (getPreferredSize [] +canvas-dimensions+))
        listener (make-input-handler (make-draw-dots-behavior model))]
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
