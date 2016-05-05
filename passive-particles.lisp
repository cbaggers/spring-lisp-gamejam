(in-package :kepler)

(defvar *particle-stream* nil)
(defparameter *particle-resolution* '(128 128))
(defparameter *starting-positions* nil)
(defparameter *starting-velocities*
  (make-c-array (make-array *particle-resolution* :initial-element (v! 0 0 0))
		:element-type :half-vec3))

(deftclass particle-gbuffer
  (positions (sample (make-texture *starting-positions*
				   :element-type :rgb32f)))
  (positions-fbo nil)
  (velocities (sample (make-texture *starting-velocities*
				    :element-type :rgb16f)))
  (velocities-fbo nil))

(deftclass (particle-system (:constructor %make-particle-system))
  (front-gbuffer (make-particle-gbuffer))
  (back-gbuffer (make-particle-gbuffer) :type particle-gbuffer)
  (front-to-back t :type boolean))

(defun make-particle-system ()
  (let ((result (%make-particle-system)))
    (let ((front (particle-system-front-gbuffer result))
	  (back (particle-system-back-gbuffer result)))
      (setf (particle-gbuffer-positions-fbo front)
	    (make-fbo `(0 ,(sampler-texture
			    (particle-gbuffer-positions front))))
	    (particle-gbuffer-velocities-fbo front)
	    (make-fbo `(0 ,(sampler-texture
			    (particle-gbuffer-velocities front)))))
      (setf (particle-gbuffer-positions-fbo back)
	    (make-fbo `(0 ,(sampler-texture
			    (particle-gbuffer-positions back))))
	    (particle-gbuffer-velocities-fbo back)
	    (make-fbo `(0 ,(sampler-texture
			    (particle-gbuffer-velocities back)))))
      result)))

(defun reset-particle-system (sys)
  (let ((front (particle-system-front-gbuffer sys))
	(back (particle-system-back-gbuffer sys)))
    (push-g *starting-positions*
	    (sampler-texture (particle-gbuffer-positions front)))
    (push-g *starting-positions*
	    (sampler-texture (particle-gbuffer-positions back)))

    (push-g *starting-velocities*
	    (sampler-texture (particle-gbuffer-velocities front)))
    (push-g *starting-velocities*
	    (sampler-texture (particle-gbuffer-velocities back)))
    sys))

(defmethod free ((object particle-system))
  (free (particle-system-front-gbuffer object))
  (free (particle-system-back-gbuffer object)))

(defmethod free ((object particle-gbuffer))
  (free (particle-gbuffer-positions object))
  (free (particle-gbuffer-velocities object)))

;;----------------------------------------------------------------

(defun init-particles ()
  (unless *particle-stream*
    (setf *particle-stream*
	  (apply #'make-particle-stream *particle-resolution*))
    (let ((arr (make-c-array nil :dimensions *particle-resolution*
			     :element-type :vec3)))
      (dbind (w h) *particle-resolution*
	(labels ((init (ptr x y)
		   ;;(declare (ignore x y))
		   (setf (cffi:mem-aref ptr :float 0) (+ -10s0 (/ x (/ w 20s0)))
			 (cffi:mem-aref ptr :float 1) (+ -10s0 (/ y (/ h 20s0)))
			 (cffi:mem-aref ptr :float 2) 0s0)))
	  (setf *starting-positions* (across-c-ptr #'init arr)))))
    t))

(defun populate-velocities-using-func (ptr-x-y-func)
  (with-c-array (arr (make-c-array nil :dimensions *particle-resolution*
				   :element-type :vec3))
    (across-c-ptr ptr-x-y-func arr)
    (push-g
     arr
     (sampler-texture
      (particle-gbuffer-velocities
       (particle-system-front-gbuffer *particle-system*))))
    (push-g
     arr
     (sampler-texture
      (particle-gbuffer-velocities
       (particle-system-back-gbuffer *particle-system*))))))

;;----------------------------------------------------------------

(defun-g particle-vert ((vert cepl:g-pt))
  (values (v! (pos vert) 1)
	  (* (v! 1 -1) (cepl:tex vert))))

(defun-g update-particle-positions ((tex-coord :vec2)
				    &uniform (positions :sampler-2d)
				    (velocities :sampler-2d)
				    (field-size :float))
  (let* ((position (texture positions tex-coord))
	 (velocity (texture velocities tex-coord))
	 (new (+ (v! (s~ position :xy) 0 0)
		 (v! (s~ velocity :xy) 0 0))))
    (v! (- (mod (+ field-size (v:x new)) (* 2 field-size)) field-size)
	(- (mod (+ field-size (v:y new)) (* 2 field-size)) field-size))))

(def-g-> move-particles ()
  #'particle-vert #'update-particle-positions)

;;----------------------------------------------------------------

(defun-g update-particle-velocities ((tex-coord :vec2)
				     &uniform (positions :sampler-2d)
				     (velocities :sampler-2d))
  (let* ((position (texture positions tex-coord))
	 (velocity (texture velocities tex-coord)))
    velocity))

(def-g-> update-velocities-pline ()
  #'particle-vert #'update-particle-velocities)

;;----------------------------------------------------------------

(defun-g place-particle ((vert :vec4) &uniform (positions :sampler-2d)
			 (cam cam-g :ubo))
  (let* ((particle-scale 0.4)
	 (pos-index (v!int (int (floor (v:z vert))) (int (floor (v:w vert)))))
	 (particle-position (texel-fetch positions pos-index 0))
	 (corner-pos (v! (v:x vert) (v:y vert))))
    (values (v! (cam-it (+ (v! (* corner-pos particle-scale) 0.8)
			   (* (v! (s~ particle-position :xy) 0) 10))
			cam)
		1)
    	    (* (+ corner-pos (v! 1 1)) 0.5)
	    (v! 1 0 1 1))))

(defun-g place-particle-frag ((tex-coord :vec2) (col :vec4)
			      &uniform (tex :sampler-2d))
  ;;col
  (texture tex tex-coord))

(def-g-> draw-particles-pline ()
  #'place-particle #'place-particle-frag)

;;----------------------------------------------------------------

(defun update-particles (particle-system)
  (let* ((quad (get-gpu-quad))
	 (f2b (particle-system-front-to-back particle-system))
	 (source (if f2b
	 	     (particle-system-front-gbuffer particle-system)
	 	     (particle-system-back-gbuffer particle-system)))
	 (destination (if f2b
			  (particle-system-back-gbuffer particle-system)
			  (particle-system-front-gbuffer particle-system))))
    ;;
    (setf (particle-system-front-to-back particle-system) (not f2b))
    ;;
    (with-fbo-bound ((particle-gbuffer-velocities-fbo destination)
    		     :with-blending nil)
      (map-g #'update-velocities-pline quad
    	     :positions (particle-gbuffer-positions source)
    	     :velocities (particle-gbuffer-velocities source)))
    ;;
    (with-fbo-bound ((particle-gbuffer-positions-fbo destination)
    		     :with-blending nil)
      (map-g #'move-particles quad
    	     :positions (particle-gbuffer-positions source)
    	     :velocities (particle-gbuffer-velocities source)
	     :field-size (field-size)))))

(defun draw-passive-particles (particle-system camera texture)
  (let* ((f2b (particle-system-front-to-back particle-system))
	 (destination (if f2b
			  (particle-system-back-gbuffer particle-system)
			  (particle-system-front-gbuffer particle-system))))
    (map-g #'draw-particles-pline *particle-stream*
	   :positions (particle-gbuffer-positions destination)
	   :tex texture
	   :cam (camera-ubo camera))))

;;----------------------------------------------------------------

(defun make-particle-stream (size-x size-y)
  ;; (v! vert.x vert.y pos.u pos.v)
  (let* ((quad-verts (vector (v! -1.0 -1.0) (v! 1.0 -1.0)
			     (v! 1.0 1.0) (v! -1.0 1.0)))
	 (verts
	  (with-c-array
	      (arr (make-c-array nil :dimensions (* 4 size-x size-y)
				 :element-type :vec4))
	    (labels ((put (ptr index)
		       (multiple-value-bind (y x) (floor (floor index 4) size-x)
			 (let ((qv (svref quad-verts (mod index 4))))
			   (setf (cffi:mem-aref ptr :float 0) (v:x qv)
				 (cffi:mem-aref ptr :float 1) (v:y qv)
				 (cffi:mem-aref ptr :float 2) (+ 0s0 x)
				 (cffi:mem-aref ptr :float 3) (+ 0s0 y))))))
	      (across-c-ptr #'put arr))
	    (make-gpu-array arr)))
	 (indices (with-c-array
		      (arr (make-c-array nil :dimensions (* 6 size-x size-y)
					 :element-type :uint))
		    (let ((indices #(3 0 1 3 1 2)))
		      (labels ((put (ptr x)
				 (multiple-value-bind (quad-num n) (floor x 6)
				   (setf (cffi:mem-aref ptr :uint)
					 (+ (aref indices n) (* quad-num 4))))))
			(across-c-ptr #'put arr)))
		    (make-gpu-array arr))))
    (make-buffer-stream verts :index-array indices :retain-arrays t)))

;;----------------------------------------------------------------