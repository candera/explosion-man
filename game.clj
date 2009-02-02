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


(def game-state (ref []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- clamp-to 
  "Returns the value x, limited between minimum (inclusive) and
maximum (exclusive)"
  [x minimum maximum]
  (min (max x minimum) (dec maximum)))

(defn- horiz
  "Returns the horizontal component of a dimension"
  [dim]
  (first dim))

(defn- vert
  "Returns the vertical component of a dimension"
  [dim]
  (second dim))

(defn- board-width 
  "Returns the width of the board in cells"
  []
  (horiz *board-dims*))

(defn- board-height
  "Returns the height of the board in cells"
  [] 
  (vert *board-dims*))

(defn- cell-width
  "Returns the width of a cell in pixels"
  []
  (horiz *cell-dims*))

(defn- cell-height
  "Returns the height of a cell in pixels"
  []
  (vert *cell-dims*))

(defn- make-cursor
  "Creates a new cursor object"
  [loc]
  {:type :cursor :location loc})

(defn- make-frame 
  "Creates the JFrame for the app."
  []
  (JFrame. "Explosion Man"))

(defn- make-random-locations
  "Creates a random collection of locations on the grid specified by [width height]"
  [[width height] density]
  (vec (distinct (for [i (range 0 (* density (* width height)))]
		   [(rand-int width) (rand-int height)]))))

(defn- make-wall
  "Creates a new wall object"
  [loc]
  {:type :wall :location loc})

(defn- initial-game-state
  "Returns a new game with a random maze of the given density"
  [density]
  (let [cursor (make-cursor (vec (map #(int (/ % 2)) *board-dims*)))] 
    (into [cursor] 
	  (remove #(= % (make-wall (:location cursor))) 
		  (map make-wall (make-random-locations *board-dims* density))))))

(defn- get-cursor
  "Returns the cursor object"
  []
  (first (filter #(= (:type %) :cursor) @game-state)))

(defn- move-cursor 
  "Moves the cursor in the specified direction"
  [cursor-loc direction]
  (vec (map clamp-to (map + cursor-loc direction) [0 0] *board-dims*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- color 
  "Creates a java.awt.Color from an rgb triplet"
  [r g b]
  (Color. (float r) (float g) (float b)))

(defmulti get-color :type)
(defmethod get-color :wall [o]  (color 1 0 0))
(defmethod get-color :cursor [o] (color 0 1 0))
(defmethod get-color :default [o] (color 0 0 0))

(defn- paint 
  "Paints the game"
  [g]
  (doseq [item @game-state]
;   (println item)
;   (println (get-color item))
    (.setColor g (get-color item))
    (.fillRoundRect
     g
     (* (horiz (:location item)) (cell-width))
     (* (vert (:location item))  (cell-height))
     (dec (cell-width))
     (dec (cell-height))
     (/ (cell-width) 5)
     (/ (cell-height) 5))))


(defn- handle-keypress 
  "Updates the game state accordingly when a key is pressed"
  [e]
  (println (format "Key pressed: %s %s %s" (:cursor @game-state)
		    (.getKeyCode e)
		    (*dirs* (.getKeyCode e))))
  (dosync 
   (let [cursor (get-cursor)
	 keycode (.getKeyCode e)
	 direction (*dirs* keycode)] 
     (if direction 
       (ref-set game-state 
		 (replace {cursor (make-cursor (move-cursor (:location cursor) direction))} 
			  @game-state))))))


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
    (dosync (ref-set game-state (initial-game-state 0.25)))
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