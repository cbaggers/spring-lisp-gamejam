(in-package #:kepler)
(in-readtable fn:fn-reader)

;;----------------------------------------------------------------------

(deftclass game-state
  (level 0)
  (stage 0))

;;----------------------------------------------------------------------

(deftclass actoroid
  ;;
  ;; gpu stuff
  (kind nil :type symbol)
  (stream (get-gpu-quad) :type buffer-stream)
  (texture (error "A sampler must be provided") :type sampler)
  (colors (vector (nrgb 100 100 100)
		  (nrgb 130 130 130)
		  (nrgb 180 180 180))
	  :type vector)
  ;;
  ;; world stuff
  (position (v! 0 0) :type rtg-math.types:vec2)
  (velocity (v! 0 0) :type rtg-math.types:vec2)
  (rotation 0s0 :type single-float)
  (mass 1s0 :type single-float)
  (radius 1s0 :type single-float)
  (invincible-for-seconds 0s0 :type single-float))

;;----------------------------------------------------------------------

(deftclass (player (:include actoroid)
		   (:constructor %make-player))
  (texture
   (sample (cepl.devil:load-image-to-texture (path "jovian_rgb.png")))
   :type sampler)
  (stuck nil :type list)
  (accel-ramp 0s0 :type single-float)
  (decel-ramp 0s0 :type single-float)
  (max-speed 1s0 :type single-float)
  (key-up-vel (v! 0 0) :type rtg-math.types:vec2)
  (key-down-vel 0s0 :type rtg-math.types:vec2))

(defun make-player ()
  (dbind (name tex &key speed radius mass) (player-stats 0 0)
    (declare (ignore name))
    (%make-player
     :texture (load-texture tex)
     :max-speed speed
     :radius radius
     :mass mass)))

(defmethod mass ((x actoroid))
  (actoroid-mass x))

(defmethod mass ((x player))
  (+ (actoroid-mass x)
     (reduce λ(+ _ (actoroid-mass (car _1))) (player-stuck x)
	     :initial-value 0s0)))

;;----------------------------------------------------------------------

(defstruct-g cam-g
  (position :vec2)
  (size :vec2)
  (zoom :float))

(deftclass (camera (:constructor %make-camera))
  (ubo (make-ubo (list (v! 0 0) (v! 800 600) 1s0) 'cam-g)
       :type ubo)
  (%zoom 10s0)
  (viewport (make-viewport '(800 600))
	    :type viewport))

(defun make-camera ()
  (let ((r (%make-camera)))
    (setf (zoom r) (camera-%zoom r))
    r))

(defun zoom (camera)
  (camera-%zoom camera))

(defun (setf zoom) (value camera)
  (setf (camera-%zoom camera) value)
  (with-gpu-array-as-c-array (x (ubo-data (camera-ubo camera)))
    (setf (cam-g-zoom (aref-c x 0))
	  value)))

(defun cam-pos (camera)
  (with-gpu-array-as-c-array (x (ubo-data (camera-ubo camera)))
    (cam-g-position (aref-c x 0))))

(defun (setf cam-pos) (value camera)
  (with-gpu-array-as-c-array (x (ubo-data (camera-ubo camera)))
    (setf (cam-g-position (aref-c x 0))
	  value)))

(defmacro-g cam-it (pos cam)
  `(* (- ,pos (v! (cam-g-position ,cam) 0))
      (v! (/ (/ (v:y (cam-g-size ,cam)) (v:x (cam-g-size ,cam)))
		(cam-g-zoom ,cam))
	     (/ 1 (cam-g-zoom ,cam))
	     1)))
