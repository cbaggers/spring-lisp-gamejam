;;;; vacuum.asd

(asdf:defsystem #:vacuum
  :description "I dun a game :p"
  :author "Baggers <techsnuffle@gmail.com>"
  :license "GPL v3"
  :depends-on (#:cepl
               #:temporal-functions
               #:cepl.sdl2
               #:swank
               #:livesupport
               #:cepl.skitter.sdl2
               #:dirt
               #:structy-defclass
               #:dendrite
               #:easing
               #:sdl2-mixer
               #:shipshape)
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "types")
               (:file "passive-particles")
               (:file "space-bodies")
               (:file "game")
               (:file "shipping")))
