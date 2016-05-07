(in-package :kepler)

(defstruct bloom-data
  c0 sc0
  c1 sc1
  c2 sc2
  c3 sc3
  h0 sh0
  h1 sh1
  h2 sh2
  h3 sh3)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun-g bloom-vert ((quad g-pt))
  (values (v! (pos quad) 1) (tex quad)))

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun-g bloom-blit-frag ((tc :vec2) &uniform (tex :sampler-2d))
  (texture tex tc))

(def-g-> bloom-blit ()
  #'bloom-vert #'bloom-blit-frag)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun-g bloom-qkern ((tc :vec2) &uniform (tex :sampler-2d) (offset :vec2))
  (+ (* (texture tex (- tc offset)) 0.3125)
     (* (texture tex tc) 0.375)
     (* (texture tex (+ tc offset)) 0.3125)))

(def-g-> bloom-smooth ()
  #'bloom-vert #'bloom-qkern)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun-g bloom-fourtex ((tc :vec2) &uniform (t0 :sampler-2d) (t1 :sampler-2d)
			(t2 :sampler-2d) (t3 :sampler-2d) (scale-effect :float))
  (+ (* (texture t0 (* tc (v! 1 -1))) 1)
     (* (texture t1 tc) scale-effect)
     (* (texture t2 tc) (/ scale-effect 2))
     (* (texture t3 tc) (/ scale-effect 4))))

(def-g-> bloom-combine ()
  #'bloom-vert #'bloom-fourtex)

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar %bloom-fbos nil)

(defun bloom-clear ()
  (clear (bloom-data-c0 %bloom-fbos))
  (clear (bloom-data-c1 %bloom-fbos))
  (clear (bloom-data-c2 %bloom-fbos))
  (clear (bloom-data-c3 %bloom-fbos))
  (clear (bloom-data-h0 %bloom-fbos))
  (clear (bloom-data-h1 %bloom-fbos))
  (clear (bloom-data-h2 %bloom-fbos))
  (clear (bloom-data-h3 %bloom-fbos)))

(defun bloom (stream texture bloom-factor)
  (bloom-clear)
  (map-g-into (bloom-data-c0 %bloom-fbos) #'bloom-blit stream :tex texture)
  (map-g-into (bloom-data-c1 %bloom-fbos) #'bloom-blit stream :tex texture)
  (map-g-into (bloom-data-c2 %bloom-fbos) #'bloom-blit stream :tex texture)
  (map-g-into (bloom-data-c3 %bloom-fbos) #'bloom-blit stream :tex texture)
  (map-g-into (bloom-data-h0 %bloom-fbos) #'bloom-smooth stream
	      :tex (bloom-data-sc0 %bloom-fbos) :offset (v! (/ 1.2 512) 0))
  (map-g-into (bloom-data-h1 %bloom-fbos) #'bloom-smooth stream
	      :tex (bloom-data-sc1 %bloom-fbos) :offset (v! (/ 1.2 256) 0))
  (map-g-into (bloom-data-h2 %bloom-fbos) #'bloom-smooth stream
	      :tex (bloom-data-sc2 %bloom-fbos) :offset (v! (/ 1.2 128) 0))
  (map-g-into (bloom-data-h3 %bloom-fbos) #'bloom-smooth stream
	      :tex (bloom-data-sc3 %bloom-fbos) :offset (v! (/ 1.2 64) 0))
  (map-g-into (bloom-data-c0 %bloom-fbos) #'bloom-smooth stream
	      :tex (bloom-data-sh0 %bloom-fbos) :offset (v! 0 (/ 1.2 512)))
  (map-g-into (bloom-data-c1 %bloom-fbos) #'bloom-smooth stream
	      :tex (bloom-data-sh1 %bloom-fbos) :offset (v! 0 (/ 1.2 256)))
  (map-g-into (bloom-data-c2 %bloom-fbos) #'bloom-smooth stream
	      :tex (bloom-data-sh2 %bloom-fbos) :offset (v! 0 (/ 1.2 128)))
  (map-g-into (bloom-data-c3 %bloom-fbos) #'bloom-smooth stream
	      :tex (bloom-data-sh3 %bloom-fbos) :offset (v! 0 (/ 1.2 64)))
  ;; (map-g #'bloom-combine stream
  ;; 	 :t0 texture :t1 (bloom-data-sc0 %bloom-fbos)
  ;; 	 :t2 (bloom-data-sc1 %bloom-fbos) :t3 (bloom-data-sc2 %bloom-fbos)
  ;; 	 :scale-effect bloom-factor)
  )

;;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun init-bloom-fbos (res)
  (when %bloom-fbos (free %bloom-fbos))
  (let* ((x1 (floor (x res)))
	 (x2 (floor (/ x1 2)))
	 (x3 (floor (/ x2 2)))
	 (x4 (floor (/ x3 2)))
	 (y1 (floor (y res)))
	 (y2 (floor (/ y1 2)))
	 (y3 (floor (/ y2 2)))
	 (y4 (floor (/ y3 2)))
	 (c0 (make-fbo `(0 :dimensions (,x1 ,y1))))
	 (sc0 (sample (attachment-tex c0 0)))
	 (c1 (make-fbo `(0 :dimensions (,x2 ,y2))))
	 (sc1 (sample (attachment-tex c1 0)))
	 (c2 (make-fbo `(0 :dimensions (,x3 ,y3))))
	 (sc2 (sample (attachment-tex c2 0)))
	 (c3 (make-fbo `(0 :dimensions (,x4 ,y4))))
	 (sc3 (sample (attachment-tex c3 0)))
	 (h0 (make-fbo `(0 :dimensions (,x1 ,y1))))
	 (sh0 (sample (attachment-tex h0 0)))
	 (h1 (make-fbo `(0 :dimensions (,x2 ,y2))))
	 (sh1 (sample (attachment-tex h1 0)))
	 (h2 (make-fbo `(0 :dimensions (,x3 ,y3))))
	 (sh2 (sample (attachment-tex h2 0)))
	 (h3 (make-fbo `(0 :dimensions (,x4 ,y4))))
	 (sh3 (sample (attachment-tex h3 0))))
    (setf %bloom-fbos
	  (make-bloom-data
	   :c0 c0 :sc0 sc0 :c1 c1
	   :sc1 sc1 :c2 c2 :sc2 sc2 :c3 c3 :sc3 sc3
	   :h0 h0 :sh0 sh0 :h1 h1 :sh1 sh1 :h2 h2 :sh2
	   sh2 :h3 h3 :sh3 sh3))))

(defmethod free ((data bloom-data))
  (free (bloom-data-c0 data))
  (free (bloom-data-c0 data))
  (free (bloom-data-c1 data))
  (free (bloom-data-c1 data))
  (free (bloom-data-c2 data))
  (free (bloom-data-c2 data))
  (free (bloom-data-c3 data))
  (free (bloom-data-c3 data))
  (free (bloom-data-h0 data))
  (free (bloom-data-h1 data))
  (free (bloom-data-h2 data))
  (free (bloom-data-h3 data))
  (free (bloom-data-sc0 data))
  (free (bloom-data-sc0 data))
  (free (bloom-data-sc1 data))
  (free (bloom-data-sc1 data))
  (free (bloom-data-sc2 data))
  (free (bloom-data-sc2 data))
  (free (bloom-data-sc3 data))
  (free (bloom-data-sc3 data))
  (free (bloom-data-sh0 data))
  (free (bloom-data-sh1 data))
  (free (bloom-data-sh2 data))
  (free (bloom-data-sh3 data))
  t)
