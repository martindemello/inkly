(ns inkling 
  (:import (javax.swing SwingUtilities JFrame JPanel WindowConstants)
           (javax.swing.event MouseInputAdapter)
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

(defn make-canvas-component [model]
  (let [p (proxy [JPanel] []
            (paintComponent [g]
              (proxy-super paintComponent g)
              (render-model model g))
            (getMinimumSize [] +canvas-dimensions+)
            (getPreferredSize [] +canvas-dimensions+))
        draw-square (fn [x y]
                      (let [poly (new Polygon)]
                        (.addPoint poly (- x 2) (- y 2))
                        (.addPoint poly (- x 2) (+ y 2))
                        (.addPoint poly (+ x 2) (+ y 2))
                        (.addPoint poly (+ x 2) (- y 2))
                        (add-polygon! model poly)))
        listener (proxy [MouseInputAdapter] []
                   (mouseDragged [e] (draw-square (.getX e) (.getY e))))]
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
