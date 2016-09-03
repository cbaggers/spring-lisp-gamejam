(in-package :vacuum)

(defun path (x)
  (local-path x :vacuum))

(defun nrgb (r g b)
  (v! (/ r 255.0)
      (/ g 255.0)
      (/ b 255.0)))

(defun rotate-v2 (x ang)
  (s~ (m3:*v (m3:rotation-z ang) (v! x 0)) :xy))

(defmacro vbind (vars value-form &body body)
  ;; {TODO} handle declare forms properly. It is complicated
  ;;        as the declare has to be the first thing in the scope
  ;;        but the vars are now split across multiple binds
  (let* ((list? (mapcar #'listp vars))
	 (mvb-vars (mapcar (lambda (v l?) (if l? (gensym) v)) vars list?))
	 (d-vars (mapcar (lambda (v l?) (when l? v)) vars list?))
	 (d-forms (mapcar (lambda (mvb d)
			    (when d `(dbind ,d ,mvb)))
			  mvb-vars d-vars))
	 (d-forms (remove nil d-forms)))
    `(multiple-value-bind ,mvb-vars ,value-form
       ,@(reduce (lambda (accum x)
		   (list (append x accum)))
		 (cons body d-forms)))))

;;----------------------------------------------------------------------
;; gpu helpers

(defun make-gpu-quad ()
  (dbind (v i) (dendrite.primitives:plain-data
                :width 2s0 :height 2s0 :normals nil)
    (make-buffer-stream
     (make-gpu-array v :element-type 'g-pt)
     :index-array (make-gpu-array i :element-type :ushort)
     :retain-arrays t)))

(defvar *gpu-quad* nil)

(defun get-gpu-quad ()
  (or *gpu-quad* (setf *gpu-quad* (make-gpu-quad))))

;;----------------------------------------------------------------------

(defparameter *cached-textures* (make-hash-table :test #'equal))

(defun load-texture (path)
  (or (gethash path *cached-textures*)
      (setf (gethash path *cached-textures*)
	    (sample (dirt:load-image-to-texture (path path))))))

(defparameter *cached-ogg* (make-hash-table :test #'equal))

(defun load-ogg (path)
  (or (gethash path *cached-textures*)
      (setf (gethash path *cached-ogg*)
	    (sdl2-mixer:load-music (path path)))))

(defparameter *cached-wavs* (make-hash-table :test #'equal))

(defun load-wav (path)
  (or (gethash path *cached-textures*)
      (setf (gethash path *cached-wavs*)
	    (sdl2-mixer:load-wav (path path)))))

(defun vec2->angle (vec2)
  (+ (atan (y vec2) (x vec2))
     (/ +pi+ 2)))
