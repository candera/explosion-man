(ns com.wangdera.explosion-man.game
  (:import (javax.swing JFrame JPanel)
	   (java.awt Dimension)
	   (java.awt.event ActionListener KeyListener)
	   (java.awt Color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *turn-length* 200) ; Length of a turn in ms
(def *board-width* 25)  ; Width of board in cells
(def *board-height* 25) ; Height of board in cells
(def *cell-width* 10)   ; Width of cell in pixels
(def *cell-height* 10)  ; Height of cell in pixelx

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-frame 
  "Creates the JFrame for the app."
  []
  (JFrame. "Explosion Man"))

(defn- paint 
  "Paints the game"
  [g]
  (dorun 
    (for [x (range 0 *board-width*), 
	  y (range 0 *board-height*)] 
      (do  
	(.setColor
	 g
	 (Color. (float (/ x *board-width*)) (float (/ y *board-height*)) (float 0.5)))
	(.fillRoundRect 
	 g 
	 (* x *cell-width*)
	 (* y *cell-height*)
	 (dec *cell-width*)
	 (dec *cell-height*)
	 (/ *cell-width* 5)
	 (/ *cell-height* 5))))))

(defn- make-panel
  "Creates the JPanel for the app."
  []
  (proxy [JPanel ActionListener KeyListener] []
    (getPreferredSize 
     [] 
     (Dimension.
      (* *board-width* *cell-width*)
      (* *board-height* *cell-height*)))
    (paintComponent
     [g]
     (proxy-super paintComponent g)
     (paint g))
    (keyPressed [e])
    (keyReleased [e])
    (keyTyped [e])))

(defn- stop? 
  "Determines if the game is over."
  [frame]
  false)

(defn- run 
  "The main loop for the app."
  [frame]
  (do 
    (.show frame)
    (loop [stop false]
     (if stop
       "Game over"
       (do 
	 (Thread/sleep *turn-length*)
	 (recur (stop? frame)))))))

(defn main []
  (let [frame (make-frame), 
	panel (make-panel)] 
    (try 
     (.add frame panel)
     (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
     (.pack frame)
     (run frame)
     (finally
      (.dispose frame)))))

(defn main-test []
  (let [frame (make-frame)
	panel (make-panel)]
    (.add frame panel)
;;     (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (.pack frame)
    (.show frame)))