;;;; kepler.asd

(asdf:defsystem #:kepler
  :description "Describe kepler here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl
               #:temporal-functions
               #:cepl.sdl2
               #:swank
               #:livesupport
               #:cepl.skitter.sdl2
               #:cepl.devil
	       #:structy-defclass
	       #:dendrite
	       #:easing
	       #:sdl2-mixer)
  :serial t
  :components ((:file "package")
	       (:file "helpers")
	       (:file "types")
	       (:file "passive-particles")
	       (:file "space-bodies")
               (:file "game")))
