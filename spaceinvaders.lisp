;;;; spaceinvaders.lisp
;;;; Main file

(in-package #:spaceinvaders)

(defparameter *assets-path* (namestring (asdf:system-relative-pathname :spaceinvaders "assets/")))

(defun draw-text (text x y color)
  "Draws a text
Args:
text: text to display
x\y: cordinates to draw it on
Color: Selected SDL color to display the text in"
  (sdl:draw-string-solid-* text x y :color color))

(defun draw-debug-text (text x y)
  "Draws text when debug is on
args:
text: text to display
x\y: cordinates to draw on"
  (when *debug*
    (draw-text text x y *debug-color*)))

(defun get-file (filename &optional (path *assets-path*))
  (concatenate 'string path filename))



(defun main ()
  "Main function"
  ;(setf *highscore* (load-scores))

  
  (sdl:with-init ()
    (create-window *width* *height* "space-invaders" *FPS*)
    (let* ((pressed-keys nil)
	   (cells '((0 0 11 7) (13 0 11 7) (26 0 11 7) (38 0 11 7)
		  (50 0 8 7) (59 0 8 7) (68 0 13 7) (82 0 13 7) (96 0 13 7) (112 0 13 7)))
	   
	   ;; 0\1 - Alien1, 2\3 - Alien2, 4\5 - Alien 3, 6 - alien death, 7 - player, 8\9 - player death
	   (sprite-sheet (make-sprite-sheet (get-file "sprites.png")  cells #(0 0)))
	   (background (sdl:load-image (get-file "invadbez.png") :color-key-at #(300 300)))
	   (player (create-player (elt sprite-sheet 7) (list (elt sprite-sheet 8) (elt sprite-sheet 9))
				  (+ (ceiling (/ *game-w-e* 2)) 10 ) *game-h-e*)))

      (setf *player-height*  (get-sprite-h player))
      (init player sprite-sheet)
      (let ((font (make-instance 'sdl:ttf-font-definition
				 :size 40
				 :filename   (get-file "FiraCode-Medium.ttf") )))

	(unless (and (not (null font))(sdl:initialise-default-font font))
	  (error "FONT-EXAMPLE: Cannot initialize the default font.")))
      
      ;; Initialize PNG
      (sdl-image:init-image :png)
      
      (sdl:enable-key-repeat 1 1)

      
      (sdl:with-events ()
	(:quit-event ()
		     (sdl-image:quit-image)
		     t)
	
	(:key-down-event (:key key)
			 
			 (when (or (sdl:key= key :sdl-key-q)
				   (sdl:key= key :sdl-key-escape))
			   (sdl:push-quit-event))
			 (when (sdl:key= key :sdl-key-r)
			   (init player sprite-sheet))
			 (setf pressed-keys (sdl:key-state-p)))
	(:key-up-event ()
		       (setf pressed-keys (sdl:key-state-p)))
	(:idle ()
	       (sdl:clear-display (get-color black))

	
	       (cond ((equal *state* 'run)
		      (game-loop pressed-keys player))
		     ((equal *state* 'wait)
		      (game-wait pressed-keys))
		     ((equal *state* 'game-over)
		      (game-over pressed-keys))
		     ((equal *state* 'game-won)
		      (game-won pressed-keys))
		     ((equal *state* 'restart)
		      (init player sprite-sheet))) ; Adds player to sprite-group))

	       ;(draw-text "testy" 0 0 (get-color green))
	       (sdl:draw-surface-at-* background 0 0)
	       
	       ;; Redraw the display
	       (sdl:update-display))))))


(defun create-exe (&key (name "space-invaders") (console nil))
  "Easy compilation for CCL"
  (setf *assets-path* "assets/")
  (let ((mode (if console :console :gui)))
    (ccl:save-application (concatenate 'string (namestring (asdf:system-relative-pathname :spaceinvaders "")) name ".exe")
			  :application-type #-deploy-console mode :toplevel-function #'main :prepend-kernel t )))
