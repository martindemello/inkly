(ns inkling 
  (:import (javax.swing SwingUtilities JFrame JPanel WindowConstants)
           (javax.swing.event MouseInputListener)
           (java.awt Dimension Color Polygon Rectangle)
           (java.awt.event MouseEvent KeyEvent KeyListener)
           (java.awt.image BufferedImage)
           java.lang.Math))

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

(def +input-events+ [:on-mouse-pressed :on-mouse-released
                     :on-mouse-moved :on-mouse-dragged
                     :on-mouse-entered :on-mouse-exited
                     :on-mouse-clicked :on-key-pressed
                     :on-key-released :on-key-typed])

(def <input-behavior> (apply create-struct +input-events+))

(defn do-nothing [behavior event] behavior)

(def +default-event-bindings+
  (mapcat (fn [n] [n do-nothing]) +input-events+))

(def +default-input-behavior+
  (apply struct-map (cons <input-behavior> +default-event-bindings+)))

(defn make-input-behavior [] +default-input-behavior+)

(defn make-input-dispatcher [event-name]
  (fn [behavior event] ((behavior event-name) behavior event)))

(defn make-input-event-multiplexer [event-name]
  (fn [meta-behavior event]
    (let [behaviors (meta-behavior :behaviors)
          dispatch (fn [behavior] ((behavior event-name) behavior event))]
      (assoc meta-behavior :behaviors (doall (map dispatch behaviors))))))

(def +input-event-multiplexers+
  (mapcat (fn [n] [n (make-input-event-multiplexer n)]) +input-events+))

(def +meta-behavior-skeleton+
  (apply struct-map (concat [<input-behavior>]
                            +input-event-multiplexers+
                            [:behaviors []])))

(defn compose-input-behaviors [& behaviors]
  (assoc +meta-behavior-skeleton+ :behaviors behaviors))

(defn with-just-xy [f]
  (fn [behavior event] (f behavior (.getX event) (.getY event))))

(defn guard-button [button f]
  (fn [behavior event]
    (if (= (.getButton event) button)
        (f behavior event)
        behavior)))

(dorun (map (fn [event-name]
              (let [event-name-string (.substring (str event-name) 1)
                    dispatch-fn-name (.concat "dispatch-" event-name-string)]
                (intern *ns* (symbol dispatch-fn-name)
                             (make-input-dispatcher event-name))))
            +input-events+))

(defn make-input-handler [initial-behavior]
  (let [behavior (atom initial-behavior)]
    (proxy [MouseInputListener KeyListener] []
      (mousePressed [e] (swap! behavior dispatch-on-mouse-pressed e))
      (mouseClicked [e] (swap! behavior dispatch-on-mouse-clicked e))
      (mouseReleased [e] (swap! behavior dispatch-on-mouse-released e))
      (mouseMoved [e] (swap! behavior dispatch-on-mouse-moved e))
      (mouseDragged [e] (swap! behavior dispatch-on-mouse-dragged e))
      (mouseEntered [e] (swap! behavior dispatch-on-mouse-entered e))
      (mouseExited [e] (swap! behavior dispatch-on-mouse-exited e))
      (keyPressed [e] (swap! behavior dispatch-on-key-pressed e))
      (keyReleased [e] (swap! behavior dispatch-on-key-released e))
      (keyTyped [e] (swap! behavior dispatch-on-key-typed e)))))

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

(defn make-clear-canvas-behavior [model]
  (let [on-key-pressed (fn [behavior event]
                         (when (= (.getKeyCode event) KeyEvent/VK_ESCAPE)
                           (clear-canvas! model))
                         behavior)]
    (assoc (make-input-behavior) :on-key-pressed on-key-pressed)))

(defn make-canvas-component [model]
  (let [p (proxy [JPanel] []
            (paintComponent [g]
              (proxy-super paintComponent g)
              (render-model model g))
            (getMinimumSize [] +canvas-dimensions+)
            (getPreferredSize [] +canvas-dimensions+))
        behaviors (compose-input-behaviors (make-clear-canvas-behavior model)
                                           (make-draw-stroke-behavior model))
        listener (make-input-handler behaviors)]
    (.setBackground p Color/WHITE)
    (add-update-fn! model (fn [rect] (.repaint p (.x rect) (.y rect)
                                                 (.width rect)
                                                 (.height rect))))
    (.addMouseListener p listener)
    (.addMouseMotionListener p listener)
    (.addKeyListener p listener)
    (.setFocusable p true)
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
