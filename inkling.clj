(ns inkling 
  (:import (javax.swing SwingUtilities JFrame JPanel WindowConstants)
           (java.awt Dimension Color)
           (java.awt.image BufferedImage)))

(def +canvas-width+ 600)
(def +canvas-height+ 500)
(def +canvas-dimensions+ (new Dimension +canvas-width+ +canvas-height+))

(defn make-canvas-component []
  (let [p (proxy [JPanel] []
            (paintComponent [g]
              (proxy-super paintComponent g)
              (.setColor g Color/WHITE)
              (.fillRect g 0 0 (.getWidth this) (.getHeight this)))
            (getMinimumSize [] +canvas-dimensions+)
            (getPreferredSize [] +canvas-dimensions+))]
    (.setBackground p Color/WHITE)
    p))

(defn make-toplevel-window []
  (let [w (new JFrame)
        v (make-canvas-component)]
    (.setTitle w "Inkling")
    (.add w v)
    (.pack w)
    (.setBackground w Color/WHITE)
    (.setDefaultCloseOperation w WindowConstants/DISPOSE_ON_CLOSE)
    (.setResizable w false)
    (.setVisible w true)
    w))

(SwingUtilities/invokeAndWait make-toplevel-window)
