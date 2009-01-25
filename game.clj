(ns com.wangdera.explosion-man.game
  (:import (javax.swing JFrame JPanel)
	   (java.awt Dimension)
	   (java.awt.event ActionListener KeyListener KeyEvent)
	   (java.awt Color))
  (:use (clojure.contrib import-static)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static imports
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *turn-length* 200)     ; Length of a turn in ms
(def *board-dims* [25 25])  ; Size of board in cells
(def *cell-dims* [40 40])   ; Size of cell in pixels

(def *dirs* {VK_LEFT  [-1 0]
	     VK_DOWN  [0 1]
	     VK_UP    [0 -1]
	     VK_RIGHT [1 0]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def game-state (ref {:cursor (map #(int (/ % 2)) *board-dims*)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- clamp-to 
  "Returns the value x, limited between minimum (inclusive) and
maximum (exclusive)"
  [x minimum maximum]
  (min (max x minimum) (dec maximum)))

(defn- board-width 
  "Returns the width of the board in cells"
  []
  (first *board-dims*))

(defn- board-height
  "Returns the height of the board in cells"
  [] 
  (second *board-dims*))

(defn- cell-width
  "Returns the width of a cell in pixels"
  []
  (first *cell-dims*))

(defn- cell-height
  "Returns the height of a cell in pixels"
  []
  (second *cell-dims*))

(defn- make-frame 
  "Creates the JFrame for the app."
  []
  (JFrame. "Explosion Man"))

(defn- paint 
  "Paints the game"
  [g]
  (dorun 
    (for [x (range 0 (board-width)), 
	  y (range 0 (board-height))] 
      (do  
	(.setColor
	 g
	 (if (= [x y] (:cursor @game-state)) 
	   (Color. 0 0 0)
	   (Color. (float (/ x (board-width))) (float (/ y (board-height))) (float 0.5))))
	(.fillRoundRect 
	 g 
	 (* x (cell-width))
	 (* y (cell-height))
	 (dec (cell-width))
	 (dec (cell-height))
	 (/ (cell-width) 5)
	 (/ (cell-height) 5))))))

(defn- handle-keypress 
  "Updates the game state accordingly when a key is pressed"
  [e]
  (println (format "Key pressed: %s %s %s" (:cursor @game-state)
		    (.getKeyCode e)
		    (*dirs* (.getKeyCode e))))
  (dosync 
   (let [cursor (:cursor @game-state)
	 keycode (.getKeyCode e)
	 direction (*dirs* keycode)] 
     (if direction 
       (alter game-state assoc :cursor 
	      (vec (map clamp-to (map + cursor direction) [0 0] *board-dims*)))))))

(defn- make-panel
  "Creates the JPanel for the app."
  []
  (proxy [JPanel ActionListener KeyListener] []
    (getPreferredSize 
     [] 
     (Dimension.
      (* (board-width) (cell-width))
      (* (board-height) (cell-height))))
    (paintComponent
     [g]
     (proxy-super paintComponent g)
     (paint g))
    (actionPerformed [e])
    (keyPressed [e] 
		(handle-keypress e)
		(.repaint this))
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

(defn- setup 
  "Sets up the frame and panel - common to both manual and 
game-related invocation."
  [exit-on-close]
  (let [frame (make-frame)
	panel (make-panel)]
    (.add frame panel)
    (.setFocusable panel true)
    (.addKeyListener panel panel)
    (if exit-on-close 
      (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))
    (.pack frame)
    (.show frame)
    [frame panel]))

(defn main []
  (let [[frame panel] (setup true)] 
    (try 
     (run frame)
     (finally
      (.dispose frame)))))

(defn main-test []
  (setup false))