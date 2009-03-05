;; ! TODO
;; * Make obstacles barriers to movement
;; * Make bomb explosion remove obstacles
;; * Make bomb explosions uncover goodies
;; * Make bomb explosions kill the player
;; * Add status bar at the side/top whatever
;; * Make the game end after a certain amount of time
;; * Change from random to specified mazes
;; * Get it to run as an applet, or at least standalone
;; 
;; ! DONE 
;; * Add obstacles
;; * Make bomb explosion stop at walls
;; * Make explosions go away when expired
;; * Make bomb explosion expand in each direction
;; * Handle shutdown - timers are sticking around and the panel is
;;   not getting created anew every time.
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
	   (java.awt.event ActionListener KeyListener KeyEvent WindowAdapter)
	   (java.awt Color))
  (:use (clojure.contrib import-static cond)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static imports
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN VK_SPACE VK_PAUSE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def turn-length 0.05)     ; Length of a turn in seconds
(def board-dims [25 25])  ; Size of board in cells
(def cell-dims [40 40])   ; Size of cell in pixels
(def explosion-speed 15)  ; Rate at which explosions expand [cells/sec]
(def explosion-lifetime 0.5) ; Time before explosions disappear [seconds] 

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
(def clock (ref {:time (float 0) :paused false}))

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

(defn- make-item [t loc & attrs]
  (let [item  {:type t :location loc :created (:time @clock)}]
    (if attrs
      (apply assoc item attrs)
      item)))

(defn- make-bomb 
  "Creates a bomb at the specified location and with the specified fuze (in seconds)."
  [loc fuze]
  (make-item ::bomb loc :fuze 5 :strength 2))

(defn- make-explosion
  "Creates an explosion from the specified bomb."
  [bomb]
  (make-item ::expanding-explosion (:location bomb) 
	     :expanding [[-1 0] [0 -1] [1 0] [0 1]] 
	     :strength (:strength bomb)
	     :speed explosion-speed))

(defn- make-random-locations
  "Creates a random collection of locations on the grid specified by [width height]"
  [[width height] density exclusions]
  (vec (difference 
	(into #{} (for [i (range 0 (* density (* width height)))]
		    [(rand-int width) (rand-int height)]))
	exclusions)))

(defn- initial-game-state
  "Returns a new game with a random maze of the given density"
  [density]
  (let [player-loc (vec (map #(int (/ % 2)) board-dims))
	wall-locs (make-random-locations board-dims density [player-loc])
	obstacle-locs (make-random-locations board-dims density (concat [player-loc] wall-locs))] 
    (vec (concat [(make-item ::player player-loc)]
		 (map #(make-item ::wall %) wall-locs)
		 (map #(make-item ::obstacle %) obstacle-locs)))))

(defn- get-player
  "Returns the player object"
  []
  (first (filter #(= (:type %) ::player) @game-state)))

(defn- wall-at?
  "Returns true if there is a wall at the specified location."
  [loc]
  (some #(and (= (:type %) ::wall)
	      (= (:location %) loc))
	@game-state))

(defn- move-player 
  "Moves the player in the specified direction."
  [player-loc direction]
  (let [proposed-location (vec (map clamp-to (map + player-loc direction) [0 0] board-dims))]
   (if (wall-at? proposed-location) 
     player-loc
     proposed-location)))

(defn- bomb-fuze 
  "Returns the remaining fuze for a given bomb."
  [bomb]
  (let [{fuze :fuze created :created} bomb] 
    (- fuze (- (:time @clock) created))))

(defn- bomb-text 
  "Returns the text that should be overload on a bomb."
  [bomb]
  (format "%.1f" (bomb-fuze bomb)))

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
  (make-explosion bomb))

(defmulti update-item "Updates items in the game" :type)

(defmethod update-item ::bomb [bomb]
  (if (fuze-expired? bomb)
    (alter game-state #(replace {bomb (explode bomb)} %))))

(defn- ready-to-expand? 
  "Returns true if the explosion is ready to expand in the directions indicated."
  [explosion]
  (> (:time @clock)
     (+ (:created explosion)
	(/ (:speed explosion)))))

(defn- in-bounds? 
  "Returns true when the proposed location is on the board"
  [[x y]]
  (and (< x (horiz board-dims))
       (> x -1)
       (< y (vert board-dims))
       (> y -1)))

(defn- expanded-explosion
  "Returns a new explosion in direction dir based on the specified explosion"
  [explosion dir]
  (let [proposed-loc (vec (map + dir (:location explosion)))] 
    (if (and (in-bounds? proposed-loc) 
	     (not (wall-at? proposed-loc))
	     (> (:strength explosion) 0))
      (make-item ::expanding-explosion proposed-loc
		 :expanding [dir]
		 :strength (dec (:strength explosion))
		 :speed (:speed explosion)))))

(defn- explosion-expansions
  "Returns the expansions of a given explosion"
  [explosion]
  (remove nil? (map #(expanded-explosion explosion %) (:expanding explosion))))

(defmethod update-item ::expanding-explosion [explosion]
  (if (ready-to-expand? explosion)
    (do
      (alter game-state (fn [v] (doall (concat v (explosion-expansions explosion))))) ; Weird, but it blows up without the vec
      (alter game-state #(replace %2 %1) {explosion (assoc explosion :type ::expanded-explosion)}))))

(defn- expired? 
  "Returns true when an explosion's lifetime has expired and it should be removed"
  [explosion]
  (> (- (:time @clock)
	(:created explosion))
     explosion-lifetime))

(defmethod update-item ::expanded-explosion [explosion]
  (if (expired? explosion) 
    (alter game-state #(remove (fn [x] (= x explosion)) %))))

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
(defmethod get-color ::player [o] (color 0 1 0))
(defmethod get-color ::bomb [o] (color 0 0 0))
(defmethod get-color ::expanding-explosion [o] (color 1 0.75 0))
(defmethod get-color ::expanded-explosion [o] (color 1 0 0.5))
(defmethod get-color ::obstacle [o] (color 0 0.25 1))
(defmethod get-color :default [o] (color 0.5 0.25 0.25))

(defmulti get-text-color :type)
(defmethod get-text-color ::bomb [o] (color 1 1 1))
(defmethod get-text-color :default [o] (color 1 1 0))

(derive ::wall ::rectangular)
(derive ::obstacle ::rectangular)
(derive ::player ::rectangular)
(derive ::bomb ::round)
(derive ::expanded-explosion ::round)
(derive ::expanding-explosion ::round)

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

(defmethod paint-item :default [g item]
  (paint-round-item g item)
  (draw-item-text g item (get-text-color item) "xx"))

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
		  (format "%.1f" (:time @clock))) 
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
  (let [player (get-player)]
    (ref-set game-state 
	     (replace {player (make-item ::player (move-player (:location player) data))} 
		      @game-state))))


(defmethod update-for-keypress ::lay-bomb [action]
  (let [player (get-player)]
    (alter game-state conj (make-bomb (:location player) 5))))

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
;;   (println (format "Key pressed: %s %s %s" (:player @game-state)
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

(defn- make-frame 
  "Creates the JFrame for the app."
  []
  (JFrame. "Explosion Man"))

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

(defn- make-window-listener
  "Creates a class that listens for events on the window."
  [timer]
  (proxy [WindowAdapter] []
    (windowClosed [e]
		  (println "Window closed"))
    (windowClosing [e]
		   (println "Window closing")
		   (.stop timer))
    (windowIconified [e]
		     (println "Window iconified"))))

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
    (.setDefaultCloseOperation frame 
			       (if exit-on-close 
				 JFrame/EXIT_ON_CLOSE
				 JFrame/DISPOSE_ON_CLOSE))
    (.addWindowListener frame (make-window-listener timer))
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
   (ref-set clock {:time (float 0) :paused false})))

(defn main-test []
  (reset-state)
  (setup false))