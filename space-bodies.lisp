(in-package :kepler)

(defparameter *ring-ratio* 1.3653333s0)
(defparameter *corona-ratio* 1.4670488s0)

;; "corona_inner_rgb.png"
;; "corona_outer_rgb.png"
;; "rings_rgb.png"

(defparameter *ambient-particles*
  `(("star_02.png" 0.7 0 (,(nrgb 107 56 22)
			   ,(nrgb 220 135 25)
			   ,(nrgb 253 221 107)))
    ("asteroid_rgb.png" 8 0.97 (,(nrgb 107 56 22)
				 ,(nrgb 220 135 25)
				 ,(nrgb 253 221 107)))
    ("rocky_planet_rgb.png" 30 0.97 (,(nrgb 107 56 22)
				      ,(nrgb 220 135 25)
				      ,(nrgb 253 221 107)))))

(defun passive-particle-spec (level)
  (elt *ambient-particles* level))


(defparameter *zoom-levels*
  '((30.0 34.0 48.0 60.0)
    (280.0 320.0 400.0 500.0)
    (800.0)))

(defparameter *player-journey*
  `(;; level 0
    ((:tiny-asteroid "asteroid_rgb.png"
		     :radius 1 :mass 2 :speed 60s0)
     (:medium-asteroid "asteroid_rgb.png"
		       :radius 2 :mass 6 :speed 60s0)
     (:large-asteroid "asteroid_rgb.png"
		      :radius 4 :mass 14 :speed 60s0)
     (:planetoid "rocky_planet_rgb.png"
		 :radius 8 :mass 28 :speed 60s0))
    ;; level 1
    ((:small-rocky-planet "rocky_planet_rgb.png"
			  :radius 16 :mass 40 :speed 180s0)
     (:medium-rocky-planet "rocky_planet_rgb.png"
			   :radius 32 :mass 80 :speed 180s0)
     (:gas-planet "jovian_rgb.png"
		  :radius 64 :mass 160 :speed 180s0)
     (:gas-giant "jovian_rgb.png"
		 :radius 192 :mass 480 :speed 180s0))

    ;; level 2
    ((:small-star "sun_rgb.png"
		  :radius 400 :mass 8000 :speed 350s0
		  :flare (("corona_inner_rgb.png"
			   1.4670488s0
			   :rotation-speed 0.1)
			  ("corona_inner_rgb.png"
			   1.4670488s0
			   :rotation-speed -0.1)))
     (:large-star "sun_rgb.png"
		  :radius 600 :mass 18000 :speed 350s0
		  :flare (("corona_inner_rgb.png"
			   1.4670488s0
			   :rotation-speed 0.1)
			  ("corona_inner_rgb.png"
			   1.4670488s0
			   :rotation-speed -0.1))))

    ;; level 3

    ;; end of demo
    ))

(defun player-stats (level stage)
  (elt (elt *player-journey* level) stage))

(defun get-zoom-for-stage (level stage)
  (elt (elt *zoom-levels* level) stage))

(defparameter *space-field-sizes*
  #(200s0
    1000s0
    2000s0))

(defun field-size (&optional level)
  (aref *space-field-sizes* (or level (game-state-level *game-state*))))

(defun stage-bodies-spec (level)
  (elt *bodies* level))


(defparameter *bodies*
  `(;; level 0
    ((60 (:tiny-asteroid "asteroid_rgb.png"
			 :radius 1 :mass 1
			 :speed (10 . 30)
			 :colors ((,(nrgb 84 63 41)
				    ,(nrgb 150 123 95)
				    ,(nrgb 255 199 140))
				  (,(nrgb 49 45 42)
				    ,(nrgb 95 91 86)
				    ,(nrgb 156 147 137))
				  (,(nrgb 41 62 84)
				    ,(nrgb 95 122 150)
				    ,(nrgb 140 196 255)))))
     ;;(:medium-asteroid "" 5)
     (50 (:comet "comet_rgb.png"
		 :radius 1.5 :mass 28
		 :rotation ,(/ +pi+ 2)
		 :speed (90 . 120)
		 :colors ((,(nrgb 9 83 145)
			    ,(nrgb 94 224 238)
			    ,(nrgb 210 255 253))
			  (,(nrgb 82 49 137)
			    ,(nrgb 207 105 222)
			    ,(nrgb 244 202 249)))))

     (3 (:planetoid "rocky_planet_rgb.png"
		    :radius 8 :mass 28 :speed 60
		    :colors ((,(nrgb 84 63 41)
			       ,(nrgb 150 123 95)
			       ,(nrgb 255 199 140))
			     (,(nrgb 49 45 42)
			       ,(nrgb 95 91 86)
			       ,(nrgb 156 147 137))
			     (,(nrgb 41 62 84)
			       ,(nrgb 95 122 150)
			       ,(nrgb 140 196 255))))))

    (;; level 1
     (30 (:planetoid "rocky_planet_rgb.png"
		     :radius 8 :mass 28
		     :speed 30
		     :colors ((,(nrgb 39 61 55)
				,(nrgb 97 175 175)
				,(nrgb 183 233 96))
			      (,(nrgb 53 39 61)
				,(nrgb 170 97 175)
				,(nrgb 96 192 233))
			      (,(nrgb 61 48 39)
				,(nrgb 175 159 97)
				,(nrgb 219 181 33)))))
     (30 (:small-rocky-planet "rocky_planet_rgb.png"
			      :radius 16 :mass 40
			      :speed 30
			      :colors ((,(nrgb 39 61 55)
					 ,(nrgb 97 175 175)
					 ,(nrgb 183 233 96))
				       (,(nrgb 53 39 61)
					 ,(nrgb 170 97 175)
					 ,(nrgb 96 192 233))
				       (,(nrgb 61 48 39)
					 ,(nrgb 175 159 97)
					 ,(nrgb 219 181 33)))))
     (10 (:medium-rocky-planet "rocky_planet_rgb.png"
			       :radius 32 :mass 80
			       :speed 30
			       :colors ((,(nrgb 39 61 55)
					  ,(nrgb 97 175 175)
					  ,(nrgb 183 233 96))
					(,(nrgb 53 39 61)
					  ,(nrgb 170 97 175)
					  ,(nrgb 96 192 233))
					(,(nrgb 61 48 39)
					  ,(nrgb 175 159 97)
					  ,(nrgb 219 181 33)))))
     (3 (:gas-planet "jovian_rgb.png"
		     :radius 50 :mass 900
		     :speed 30
		     :flare (("rings_rgb.png" 1.3653333s0 :at-back nil))
		     :flare-chance-in-% 60
		     :colors ((,(nrgb 107 56 22)
				,(nrgb 220 135 25)
				,(nrgb 253 221 107))
			      (,(nrgb 23 22 107)
				,(nrgb 144 25 220)
				,(nrgb 56 240 241))
			      (,(nrgb 107 22 70)
				,(nrgb 220 25 103)
				,(nrgb 253 107 134))
			      (,(nrgb 63 107 22)
				,(nrgb 87 220 25)
				,(nrgb 122 253 107))))))

    (;; level 2
     (1 (:gas-planet "jovian_rgb.png"
		     :radius 1.5 :mass 2000
		     :speed 30
		     :colors ((,(nrgb 107 56 22)
				,(nrgb 220 135 25)
				,(nrgb 253 221 107))
			      (,(nrgb 23 22 107)
				,(nrgb 144 25 220)
				,(nrgb 56 240 241))
			      (,(nrgb 107 22 70)
				,(nrgb 220 25 103)
				,(nrgb 253 107 134))
			      (,(nrgb 63 107 22)
				,(nrgb 87 220 25)
				,(nrgb 122 253 107))))))))

(defun parse-speed (x)
  (rotate-v2
   (v! 0 (etypecase x
	   (cons (dbind (s . e) x
		   (let ((range (- e s)))
		     (if (> range 0s0)
			 (+ s (random (float range)))
			 (+ s (float range))))))
	   (number (+ (random (float x))))))
   (random (* 2 +pi+))))
