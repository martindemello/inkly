; org.inkscape.inkly.input - input handling abstractions
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
(ns org.inkscape.inkly.input
  (:import (javax.swing.event MouseInputListener)
           (java.awt.event KeyListener)))

(def #^{:private true} +input-events+
  [:on-mouse-pressed :on-mouse-released
   :on-mouse-moved :on-mouse-dragged
   :on-mouse-entered :on-mouse-exited
   :on-mouse-clicked :on-key-pressed
   :on-key-released :on-key-typed])

(def #^{:private true} <input-behavior>
  (apply create-struct +input-events+))

(defn- do-nothing [behavior event] behavior)

; simple behaviors
(defn- process-event [behavior event-name event]
  ((behavior event-name) behavior event))

(def #^{:private true} +behavior-skeleton+
  (let [bindings (mapcat #(vector % do-nothing) +input-events+)]
    (apply struct-map (cons <input-behavior> bindings))))

; composite behaviors
(defn- make-event-delegate [event-name]
  (fn [meta-behavior event]
    (let [behaviors (meta-behavior :behaviors)
          delegate-to #(process-event % event-name event)
          behaviors (doall (map delegate-to behaviors))]
      (assoc meta-behavior :behaviors behaviors))))

(def #^{:private true} +meta-behavior-skeleton+
  (let [bindings (mapcat #(vector % (make-event-delegate %)) +input-events+)]
    (apply struct-map (concat [<input-behavior>] bindings [:behaviors []]))))

; define dispatch-* functions for make-input-listener
(defn- make-input-dispatcher [event-name]
  (fn [behavior event] (process-event behavior event-name event)))

(defn- define-dispatcher [event-name]
  (let [event-name-string (.substring (str event-name) 1)
        fn-name (.concat "dispatch-" (.substring (str event-name) 1))
        sym (with-meta (symbol fn-name) {:private true})]
    (intern *ns* sym (make-input-dispatcher event-name))))

(dorun (map define-dispatcher +input-events+))

; public API
(defn make-input-behavior [& args]
  (apply assoc (cons +behavior-skeleton+ args)))

(defn compose-input-behaviors [& behaviors]
  (assoc +meta-behavior-skeleton+ :behaviors behaviors))

(defn make-input-listener [initial-behavior]
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
