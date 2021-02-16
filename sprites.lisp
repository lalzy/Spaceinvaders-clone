;;;; sprites.lisp

(in-package #:spaceinvaders)

(defun make-missile-sprite (&aux
			      (w 1)
			      (h 2)
			      (new-surface (sdl:create-surface w h)))
  (sdl:draw-box-* 0 0 w h :surface new-surface :color (get-color green))
  new-surface)


(defun make-box-sprite (w h color &aux
				    (new-surface (sdl:create-surface w h)))
  (sdl:draw-box-* 0 0 w h :surface new-surface :color color)
  new-surface)

(defun sub-image (image cell &optional (color-key-pos nil))
  "Subsects image cells into seperate surfaces"
  (let* ((x (elt cell 0))
	 (y (elt cell 1))
	 (w (elt cell 2))
	 (h (elt cell 3))
	 (new-surface (sdl:create-surface w h :color-key-at color-key-pos)))
  (setf (sdl:cells image) (sdl:rectangle :x x :y y :h h :w w))
  (sdl:draw-surface image :surface new-surface)
  new-surface))


(defun make-sprite-sheet (image-path cells &optional (color-key-pos))
  "Returns a list of sprite surfaces
Image-path - Path to an image
cells - list of lists with the size of cells, ex:
   ((0 0 32 32) (0 32 32 32) (0 64 32 32))"
  (let* ((image (sdl:load-image image-path :color-key-at color-key-pos))
	 (sprites (loop for cell in cells collect (sub-image image cell color-key-pos))))
    sprites))
