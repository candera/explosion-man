;; ! TODO
;; * Make bomb explosion expand in each direction
;; * Make bomb explosion stop at walls
;; * Add destroyable obstacles
;; * Make bomb explosion remove obstacles
;; * Handle shutdown - timers are sticking around and the panel is
;;   not getting created anew every time.
;; 
;; ! DONE 
;; * Make bomb explode
;; * Make bomb and disappear at zero (no explosion)
;; * Make bomb count down to zero
;; * Add ability to drop bomb by pressing a key (no behavior)
;; * Read something on git that explains the bits I don't understand
;; * Move the highlighted square through the maze (collision detection)
;; * Display a random maze
;; * Figure out why the proxy isn't working 
;; * Get board to paint with simple colored boxes in a grid
;; * Handle a keypress and make it move a highlighted square around the
;;   board. 

(ns com.wangdera.explosion-man.game
  (:import (javax.swing JFrame JPanel Timer)
	   (java.awt Dimension)
	   (java.awt.event ActionListener KeyListener KeyEvent)
	   (java.awt Color))
  (:use (clojure.contrib import-static cond)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static imports
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN VK_SPACE VK_PAUSE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def turn-length 1)     ; Length of a turn in seconds
(def board-dims [25 25])  ; Size of board in cells
(def cell-dims [40 40])   ; Size of cell in pixels

(def key-map {VK_LEFT  [::movement [-1 0]]
	      VK_DOWN  [::movement [0 1]]
	      VK_UP    [::movement [0 -1]]
	      VK_RIGHT [::movement [1 0]]
	      VK_SPACE [::lay-bomb]
	      VK_PAUSE [::toggle-paused]})


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def game-state (ref []))
(def clock (ref {:time 0 :paused false}))

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
  (horiz board-dims))

(defn- board-height
  "Returns the height of the board in cells"
  [] 
  (vert board-dims))

(defn- cell-width
  "Returns the width of a cell in pixels"
  []
  (horiz cell-dims))

(defn- cell-height
  "Returns the height of a cell in pixels"
  []
  (vert cell-dims))

(defn- make-item [t loc]
  {:type t :location loc :created (:time @clock)})

(defn- make-bomb 
  "Creates a bomb at the specified location and with the specified fuze (in seconds)."
  [loc fuze]
  (assoc (make-item ::bomb loc) :fuze 20))

(defn- make-frame 
  "Creates the JFrame for the app."
  []
  (JFrame. "Explosion Man"))

(defn- make-random-locations
  "Creates a random collection of locations on the grid specified by [width height]"
  [[width height] density]
  (vec (distinct (for [i (range 0 (* density (* width height)))]
		   [(rand-int width) (rand-int height)]))))


(defn- initial-game-state
  "Returns a new game with a random maze of the given density"
  [density]
  (let [cursor (make-item ::cursor (vec (map #(int (/ % 2)) board-dims)))] 
    (into [cursor] 
	  (remove #(= % (make-item ::wall (:location cursor))) 
		  (map #(make-item ::wall %) (make-random-locations board-dims density))))))

(defn- get-cursor
  "Returns the cursor object"
  []
  (first (filter #(= (:type %) ::cursor) @game-state)))

(defn- wall-at?
  "Returns true if there is a wall at the specified location."
  [loc]
  (some #(and (= (:type %) ::wall)
	      (= (:location %) loc))
	@game-state))

(defn- move-cursor 
  "Moves the cursor in the specified direction."
  [cursor-loc direction]
  (let [proposed-location (vec (map clamp-to (map + cursor-loc direction) [0 0] board-dims))]
   (if (wall-at? proposed-location) 
     cursor-loc
     proposed-location)))

(defn- bomb-fuze 
  "Returns the remaining fuze for a given bomb."
  [bomb]
  (let [{fuze :fuze created :created} bomb] 
    (- fuze (- (:time @clock) created))))

(defn- bomb-text 
  "Returns the text that should be overload on a bomb."
  [bomb]
  (str (int (bomb-fuze bomb))))

(defn- fuze-expired? 
  "Returns true if the fuze of a given bomb has expired."
  [bomb]
  (< (bomb-fuze bomb) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game state update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- explode 
  "Returns the explosion that a given bomb produces."
  [bomb]
  (make-item ::explosion (:location bomb)))

(defmulti update-item "Updates items in the game" :type)

(defmethod update-item ::bomb [bomb]
  (if (fuze-expired? bomb)
    (do 
      (alter game-state #(replace {bomb (explode bomb)} %)))))

(defmethod update-item :default [item]) ; Do nothing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- color 
  "Creates a java.awt.Color from an rgb triplet"
  [r g b]
  (Color. (float r) (float g) (float b)))

(defn- item-bounds [item]
  {:x (* (horiz (:location item)) (cell-width))
   :y (* (vert (:location item))  (cell-height))
   :width (dec (cell-width))
   :height (dec (cell-height))})

(defn- bounds-center [bounds]
  {:x (+ (/ (:width bounds) 2)
	 (:x bounds))
   :y (+ (/ (:height bounds) 2)
	 (:y bounds))})

(defn- draw-text
  "Draws the text text in color color at location loc"
  [g text color loc]
  (doto g
    (.setColor color)
    (.drawString text
		 (int (horiz loc))
		 (int (vert loc)))))

(defn- draw-centered-text
  "Draws text in color centered at the loc"
  [g text clr loc]
  (let [font (.getFont g) 
	m (.getFontMetrics g font)
	h (.getHeight m)
	w (.stringWidth m text)
	cx (- (horiz loc) (/ w 2))
	cy (+ (vert loc) (/ h 2))]
    (draw-text g text clr [cx cy])))

(defn- draw-item-text 
  "Draws text text in color color bounded by the item."
  [g item color text]
  (let [bounds (item-bounds item)
	center (bounds-center bounds)]
    (draw-centered-text g text color [(:x center) (:y center)])))

(defmulti get-color :type)
(defmethod get-color ::wall [o]  (color 1 0 0))
(defmethod get-color ::cursor [o] (color 0 1 0))
(defmethod get-color ::bomb [o] (color 0 0 0))
(defmethod get-color ::explosion [o] (color 1 1 0))
(defmethod get-color :default [o] (color 0 0 0))

(defmulti get-text-color :type)
(defmethod get-text-color ::bomb [o] (color 1 1 1))
(defmethod get-text-color :default [o] (color 1 1 0))

(derive ::wall ::rectangular)
(derive ::cursor ::rectangular)
(derive ::bomb ::round)
(derive ::explosion ::round)

(defmulti paint-item (fn [g item] (:type item)))

(defmethod paint-item ::rectangular [g item]
  (let [bounds (item-bounds item)] 
    (.fillRoundRect
     g
     (:x bounds)
     (:y bounds)
     (:width bounds)
     (:height bounds)
     (/ (cell-width) 5)
     (/ (cell-height) 5))))

(defn- paint-round-item [g item]
  (let [bounds (item-bounds item)] 
    (.fillOval 
     g
     (:x bounds)
     (:y bounds)
     (:width bounds)
     (:height bounds))))


(defmethod paint-item ::bomb [g bomb]
  (paint-round-item g bomb)
  (draw-item-text g bomb (get-text-color bomb) (bomb-text bomb)))

(defmethod paint-item ::round [g item]
  (paint-round-item g item))

(defn- paint 
  "Paints the game"
  [g]
  (doseq [item @game-state]
;   (println item)
;   (println (get-color item))
    (doto g
      (draw-text (str 
		  (if (:paused @clock)
		    "PAUSED:"
		    "") 
		  (:time @clock)) 
		 (color 0 1 1) 
		 [20 20])
      (.setColor (get-color item))
      (paint-item item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keypress stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- update-for-keypress-dispatch
  "Dispatch function for the update-for-keypress multimethod"
  ([] nil)
  ([action] action)
  ([action data] action))

(defmulti update-for-keypress update-for-keypress-dispatch)

(defmethod update-for-keypress ::movement [action data]
  (let [cursor (get-cursor)]
    (ref-set game-state 
	     (replace {cursor (make-item ::cursor (move-cursor (:location cursor) data))} 
		      @game-state))))


(defmethod update-for-keypress ::lay-bomb [action]
  (let [cursor (get-cursor)]
    (alter game-state conj (make-bomb (:location cursor) 5))))

(defmethod update-for-keypress ::toggle-paused [action]
  (alter clock assoc :paused (not (:paused @clock))))

(defmethod update-for-keypress :default 
  ([] "Ignoring keypress")
  ([action] "Ignoring keypress")
  ([action data]
     (println "Ignoring keypress")))

(defn- handle-keypress 
  "Updates the game state accordingly when a key is pressed"
  [e]
;;   (println (format "Key pressed: %s %s %s" (:cursor @game-state)
;; 		    (.getKeyCode e)
;; 		    (*dirs* (.getKeyCode e))))
  (dosync (apply update-for-keypress (key-map (.getKeyCode e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timer handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- update-clock [t]
  (if (not (:paused @clock))
    (alter clock assoc :time (+ (:time @clock) t))))

(defn- handle-timer-event
  "Handles the firing of the timer event."
  [e panel]
  (dosync 
   (update-clock turn-length)
   (if (not (:paused @clock))
     (doseq [item @game-state] (update-item item))))
  (.repaint panel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Swing stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (actionPerformed [e]
		     (handle-timer-event e this))
    (keyPressed [e] 
		(handle-keypress e)
		(.repaint this))
    (keyReleased [e])
    (keyTyped [e])))

(defn- make-timer
  "Creates the timer that drives the game loop."
  [panel]
  (new Timer (int (* 1000 turn-length)) panel))

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
       (do 
	 (.hide frame)
	 "Game over")
       (do 
	 (Thread/sleep turn-length)
	 (recur (stop? frame)))))))

(defn- setup 
  "Sets up the frame and panel - common to both manual and 
game-related invocation."
  [exit-on-close]
  (let [frame (make-frame)
	panel (make-panel)
	timer (make-timer panel)]
    (dosync (ref-set game-state (initial-game-state 0.25)))
    (.add frame panel)
    (.setFocusable panel true)
    (.addKeyListener panel panel)
    (if exit-on-close 
      (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))
    (.pack frame)
    (.show frame)
    (.start timer)
    [frame panel]))

(defn main []
  (let [[frame panel] (setup true)] 
    (try 
     (run frame)
     (finally
      (.dispose frame)))))

(defn- reset-state 
  "Returns the game state to a default"
  []
  (dosync
   (ref-set game-state {})
   (ref-set clock {:time 0 :paused false})))

(defn main-test []
  (reset-state)
  (setup false))