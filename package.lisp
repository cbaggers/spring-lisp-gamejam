;;;; package.lisp

(defpackage kepler
  (:use #:cl #:cepl #:temporal-functions
	#:varjo-lang #:rtg-math #:structy-defclass
	#:dendrite #:cepl-utils #:skitter.sdl2.mouse-buttons
	#:skitter.sdl2.keys
	#:livesupport #:named-readtables #:rtg-math.base-maths)
  (:import-from #:rtg-math.maths
		:lerp))
