;;;; gamelogic.lisp
;;;; Where all the game logic is

(in-package #:spaceinvaders)

;;; Movement
(defun player-move (keys player)
  (when player
    (when (member :sdl-key-left keys)
      (decf (get-sprite-x player) (player-speed player)))
    (when (member :sdl-key-right keys)
      (incf (get-sprite-x player) (player-speed player)))))

(defun change-animation (sprite)
  (let ((animations (get-animation-set sprite)))
    (if (equal (get-sprite sprite) (first animations))
	(setf (get-sprite sprite) (second animations))
	(setf (get-sprite sprite) (first animations)))))

(defun move-invader (sprite)
  (when sprite
    (if (equal *invader-direction* 'left)
	(decf (get-sprite-x sprite) *invader-speed*)
	(incf (get-sprite-x sprite) *invader-speed*))))

(defun invader-move ()
  (dolist (invader-row *invaders*)
    (dolist (invader invader-row)
      (when (= *animation-delay* 0)
	    (change-animation invader))
      (move-invader invader))))

(defun missile-move ()
  (when *missiles*
    (dolist (missle *missiles*)
      (decf (get-sprite-y missle) *missile-speed*))))


(defun out-of-screen-p (sprite  &key
				 (reduce-size nil)
				 (invader-bottom nil))
  (when sprite
    (let ((w (if reduce-size
		 (- *game-w-e* (get-sprite-w sprite))
		 (+ (get-sprite-w sprite) *width*)))
	  (h (if reduce-size
		 (- *game-h-e* (+ (get-sprite-h sprite) *player-height*))
		 (+ (get-sprite-h sprite) *height*))))
      
      (cond ((<= (get-sprite-x sprite) *game-w-s*) 'left-edge)
	    ((<= (get-sprite-y sprite) *game-h-s*) 'top-edge)
	    ((>= (get-sprite-x sprite) w) 'right-edge)
	    ((>= (get-sprite-y sprite) h) 'bottom-edge)
	    (t nil)))))


(defun remove-missile-p (missile)
  (when (equal (type-of missile) 'missile) 
    (cond ((equal (out-of-screen-p missile) 'top-edge) t)
	  ((missile-hit-p missile) t)
	  (t nil))))

(defun remove-missiles ()
  "Deletes missile"
  (setf *sprite-group* (remove-if #'remove-missile-p *sprite-group*))
  (setf *missiles* (remove-if #'remove-missile-p *missiles*)))


;;; Collision

(defun wall-collision (player)
  (when player
    (when (<= (get-sprite-x player) *game-w-s*)
      (setf (get-sprite-x player) *game-w-s*))
    (when (>= (get-sprite-x player) (- *game-w-e* (get-sprite-w player)))
      (setf (get-sprite-x player) (- *game-w-e* (get-sprite-w player))))))


(defun move-down-a-row ()
  (dolist (invader-row *invaders*)
    (dolist (invader invader-row)
      (incf (get-sprite-y invader) (* (get-sprite-h invader) 3)))))

(defun change-invader-direction ()
  (if (equal *invader-direction* 'left)
      (setf *invader-direction* 'right)
      (setf *invader-direction* 'left)))

(defun wall-collison-invaders-p (invaders &aux (wall-hit nil))
  (when invaders
    (iter (for invader-row in invaders)
	  (let ((hit-state (or (out-of-screen-p (first invader-row)  :reduce-size t :invader-bottom t)
			    (out-of-screen-p (first (last invader-row)) :reduce-size t :invader-bottom t))))
	    (when hit-state
	      (when (equal hit-state 'bottom-edge)
		(setf *player-dead* t))
	      (setf wall-hit t)
	      (incf *invader-speed*)
	      (finish))))
    (when wall-hit
      (move-down-a-row)
      (change-invader-direction))))
  
(defun remove-invaders ()
  (labels ((remove-check (item)
	     (if (equal (type-of item) 'invader)
		   (invader-hit-p item)
		 nil)))
    (setf *sprite-group* (remove-if #'remove-check *sprite-group*))
    (setf *invaders* (iter (for invader-row in *invaders*)
			   (collect (remove-if #'remove-check invader-row))))
  ;  (setf *invaders* (remove-if #'remove-check *invaders*))
    ))

(defun invader-missile-collision-p (invader missile)
  (when (and invader missile)
    (if (and (and (>= (+ (get-sprite-x missile) (get-sprite-w missile)) (get-sprite-x invader))
		  (<= (get-sprite-x missile)
		      (+ (get-sprite-x invader) (get-sprite-w invader))))
	     
	     (and (>= (get-sprite-y missile) (get-sprite-y invader))
		  (<= (+ (get-sprite-y missile) (get-sprite-h missile))
		      (+ (get-sprite-y invader) (get-sprite-h invader)))))
	  t
	nil)))

(defun enemy-hit-check ()
  (dolist (invader-row *invaders*)
    (dolist (invader invader-row)
      (dolist (missile *missiles*)
	(when (invader-missile-collision-p invader missile)
	  (setf (missile-hit-p missile) t)
	  (incf *invaders-killed*)
	  (setf (get-sprite invader) (get-death-sprite invader))
	  (setf (invader-hit-p invader) t))))))

;;; Drawing
(defun draw-sprites (objects)
  (when objects
    (iter (for object in objects) 
	  (sdl:draw-surface-at-* (get-sprite object)
				 (get-sprite-x object)
				 (get-sprite-y object)))))
  
;;; Game effect
(defun shoot ( keys player)
  (when (and (member :sdl-key-z keys) (> *max-hold* 0))
    (make-missile player)))

(defun shoot-delay (keys)
  (if (member :sdl-key-z keys)
      (decf *max-hold*)
      (setf *max-hold* 1)))

(defun game-over (pressed-keys)
  (draw-text "Game over!" 260 350 (get-color red))
  (draw-text "Press x to exit" 190 380 (get-color red))
  (draw-text "press r to restart" 190 410 (get-color red))
  (when (member :sdl-key-x pressed-keys)
    (sdl:push-quit-event))
  (when (member :sdl-key-r pressed-keys)
    (setf *state* 'restart)))

(defun game-won (pressed-keys)
  (draw-text "You Won! press x to exit" 180 350 (get-color green))
  (draw-text "or press r to restart" 220 380 (get-color green))
  (when (member :sdl-key-x pressed-keys)
    (sdl:push-quit-event))
  (when (member :sdl-key-r pressed-keys)
    (setf *state* 'restart)))

(defun game-wait (pressed-keys)
  (draw-text "press space to start!" 130 350 (get-color green))
  (draw-text "Z key to shoot" 200 400 (get-color green))
  (draw-text "arrow keys to move" 180 440 (get-color green))
  (when (member :sdl-key-space pressed-keys)
    (setf *state* 'run)))


(defun death-animation (player)
  (if (equal (get-sprite player) (first (get-animation-set player)))
      (setf (get-sprite player) (second (get-animation-set player)))
      (setf (get-sprite player) (first (get-animation-set player)))))

(defun player-death (player)
  (cond ((= *death-time* 0)
	 (setf *state* 'game-over))
	((<= *animation-delay* 0)
	 (death-animation player)
	 (setf *animation-delay* 5)
	 )
	(t (decf *animation-delay*)
	   (decf *death-time*))))

(defun game-loop (pressed-keys player)
  (unless *player-dead*
    (progn
      (let ((missile (shoot pressed-keys player)))
	(when missile
	  (push missile *missiles*)
	  (push missile *sprite-group*)
	  )
	
	(shoot-delay pressed-keys)
	(player-move pressed-keys player)
	(missile-move)
	(when (member :sdl-key-s pressed-keys)
	  (if *stop*
	      (setf *stop* nil)
	      (setf *stop* t)))
	
	
	(invader-move)
	(wall-collison-invaders-p *invaders* )

	(when (>= *invader-speed* 6)
	  (setf *invader-speed* 6))
	
	(wall-collision player)
	(enemy-hit-check)
	
	(when (= *invaders-killed* *invaders-to-kill*)
	  (decf *invaders-killed* *invaders-to-kill*)
	  (incf *invader-speed*))
	
	(when (every #'null *invaders*)
	  (setf *state* 'game-won)))))
  
  (draw-sprites *sprite-group*)
  
  
  (when *player-dead*
    (missile-move)
    (player-death player))
  
  (remove-missiles)
  (when (= *remove-delay* 0)
    (remove-invaders)
    (setf *remove-delay* 5))
  
  (if (and (= *animation-delay* 0) (not *player-dead*))
      (setf *animation-delay* (floor (- 25 (* *invader-speed* 2))))
      (decf *animation-delay*))
  
  (decf *remove-delay*))
