;;;; init.lisp
;;;; Initialize stuff


(in-package #:spaceinvaders)

(defmacro get-color (color)
  "Returns a chosen color from the list of SDL colors found in *colors*"
  `(cadr (assoc ',color *colors*)))

(defparameter *colors* `((white ,(sdl:color :r 255 :g 255 :b 255))
		   (black ,(sdl:color :r 0 :g 0 :b 0))
		   (darkgrey ,(sdl:color :r 40 :g 40 :b 40))
		   (lightgrey ,(sdl:color :r 100 :g 100 :b 100))
		   (green ,(sdl:color :r 0 :g 255 :b 0))
		   (red ,(sdl:color :r 255 :g 0 :b 0))
		   (blue ,(sdl:color :r 0 :g 0 :b 255))
		   (yellow ,(sdl:color :r 255 :g 255 :b 0))))


;;;  Constants
(defparameter *player-dead* nil)
(defparameter *game-w-s* 90)
(defparameter *game-w-e* 695)
(defparameter *game-h-s* 240)
(defparameter *game-h-e* 650)
(defparameter *height* 800)
(defparameter *width* 770)
(defparameter *FPS* 60)
(defparameter *font1*  (sdl:initialise-font sdl:*font-10x20*))


;; Debug stuff
(defparameter *debug-font* (sdl:initialise-font sdl:*font-8x8*))
(defparameter *debug-color* (get-color green))
(defparameter *debug* t)

#|| 

line commentz

||#
(defparameter *sprite-group* nil) ; Group of sprites to draw

(defparameter *player-height* nil) ; Used when checking invader finish-line \ gameover


;;; Game stuff
(defparameter *death-time* 120)
(defparameter *animation-delay* 5) ; delay for animations
(defparameter *remove-delay* 5) ; sprite-removal delay(for death "animation")
(defparameter *invaders-killed* 0) ; how many invaders killed
(defparameter *invaders-to-kill* 150) ; how many invaders to kill before speed up
(defparameter *state* 'run) ; Gamestate
(defparameter *invader-direction* 'right) ; Direction the invaders are moving
(defparameter *missile-speed* 6) ; Speed of missiles
(defparameter *invader-speed* 1)  ; invaders horizontal speed
(defparameter *missiles* nil) ; list of missiles on screen
(defparameter *invaders* nil) ; list of invaders in rows
(defparameter *max-hold* 1) ; Checks for key-held down when shooting(to stop it)



(defun init (player sprite-sheet)
  "Initialize gameplay variables between sessions"
  (setf *death-time* 120)
  (setf *animation-delay* 5)
  (setf *remove-delay* 5)
  (setf *state* 'wait)
  (setf *invaders-killed* 0)
  (setf *invader-speed* 1)
  (setf *missiles* nil)
  (setf *sprite-group* nil)
  (setf *player-dead* nil)
  (setf *invaders* nil)
  (setf (get-sprite player) (elt sprite-sheet 7))
  
  (let* ((invader-type 1)
	 (invaders (iter (for r from 1 to 6)
			 (collect
			     (iter
			       (for x from 1 to 13)
			       (collect
				   (create-invader
				    (elt sprite-sheet invader-type) (elt sprite-sheet 6)
				    (list (elt sprite-sheet (1- invader-type)) (elt sprite-sheet invader-type))
				    (+ *game-w-s* (* x 20)) (+ (+ *game-h-s* 40) (* r 20))))))
			 
			 (when (evenp r) (incf invader-type 2)))))
    
    (dolist (invader-row invaders)
      (dolist (invader invader-row)
	(push invader *sprite-group*))
      (push invader-row *invaders*))
    (push player *sprite-group*)))

(defun create-window (h w title fps)
"Creates the game window"
  (sdl:window h w :title-caption title :FPS (make-instance 'sdl:fps-fixed :target-frame-rate fps)  :Double-buffer t))


;; Classes

(defclass sprite ()
  ((sprite
    :initarg :sprite
    :accessor get-sprite)
   (x
    :initarg :x
    :accessor get-sprite-x)
   (y
    :initarg :y
    :accessor get-sprite-y)
   (w
    :initarg :w
    :accessor get-sprite-w)
   (h
    :initarg :h
    :accessor get-sprite-h)
   (animation-set
    :initarg :sprite-animations
    :reader get-animation-set)))

(defclass player (sprite)
  ((player-speed
    :initarg :speed
    :accessor player-speed)))

(defclass invader (sprite)
  ((hit?
    :initform nil
    :accessor invader-hit-p)
   (death-sprite
    :initarg :death-sprite
    :reader get-death-sprite)))

(defclass missile (sprite)
  ((hit?
    :initform nil
    :accessor missile-hit-p)))

(defun create-player (sprite sprite-animations &optional (x 0) (y 0))
  (make-instance 'player :sprite sprite :w (sdl:width sprite) :h (sdl:height sprite)
		 :x x :y y :speed 4 :sprite-animations sprite-animations))
 
(defun create-invader (sprite death-sprite sprite-animations &optional (x 0) (y 0))
  (make-instance 'invader :sprite sprite :w (sdl:width sprite) :h (sdl:height sprite)
		 :x x :y y :death-sprite death-sprite :sprite-animations sprite-animations))


(defun make-missile (player &aux (sprite (make-missile-sprite)))
  (make-instance 'missile :sprite sprite :w (sdl:width sprite)
		 :h (sdl:height sprite)
		 :x (+ (get-sprite-x player) (ceiling (/ (get-sprite-w player) 2)))
		 :y (- (get-sprite-y player) (ceiling (/ (get-sprite-h player) 2)))))
