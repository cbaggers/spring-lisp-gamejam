(in-package #:kepler)
(in-readtable fn:fn-reader)

(defun more-rocks ()
  (setf *rocks*
	(append *rocks*
		(loop :for i :below 50 :collect
		   (make-actoroid :position (v! (+ -40s0 (random 80))
						(+ -40s0 (random 80)))
				  :velocity (v! (- (random 1s0) 0.5)
						(- (random 1s0) 0.5)))))))

(defun more-dense-rocks ()
  (setf *rocks*
	(append *rocks*
		(loop :for i :below 5 :collect
		   (make-actoroid :texture *moon-tex*
				  :position (v! (+ -40s0 (random 80))
						(+ -40s0 (random 80)))
				  :velocity (v! (- (random 1s0) 0.5s0)
						(- (random 1s0) 0.5s0))
				  :mass 10.0
				  :radius 3s0)))))
