;;;; spaceinvaders.asd


(asdf:defsystem #:spaceinvaders
  :description "A very simple spaceinvaders clone"
  :author "sknTheLisper"
  :license "MIT License"
  :depends-on (#:cffi
	       #:lispbuilder-sdl
               #:lispbuilder-sdl-ttf
	       #:lispbuilder-sdl-image
	       #:iterate)
  :serial t
  :components ((:file "package")
	       (:file "init")
	       (:file "sprites")
	       (:file "gamelogic")
               (:file "spaceinvaders")))
