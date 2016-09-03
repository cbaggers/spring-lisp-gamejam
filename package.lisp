;;;; package.lisp

(defpackage vacuum
  (:use #:cl #:cepl #:temporal-functions
        #:varjo-lang #:rtg-math #:structy-defclass
        #:dendrite #:cepl-utils #:skitter.sdl2.mouse-buttons
        #:skitter.sdl2.keys #:shipshape
        #:livesupport #:named-readtables #:rtg-math.base-maths)
  (:import-from #:rtg-math.maths
                :lerp)
  (:export :vacuum))
