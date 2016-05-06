(in-package :kepler)

(defparameter *ring-ratio* 1.3653333s0)
(defparameter *corona-ratio* 1.4670488s0)

;; "corona_inner_rgb.png"
;; "corona_outer_rgb.png"
;; "rings_rgb.png"

(defparameter *zoom-levels*
  '((30.0 34.0 48.0 60.0)
    (280.0 320.0 400.0 500.0)
    (800.0)))

(defparameter *player-journey*
  `(;; level 0
    ((:tiny-asteroid "asteroid_rgb.png"
		     :radius 1 :mass 2 :speed 1s0)
     (:medium-asteroid "asteroid_rgb.png"
		       :radius 2 :mass 6 :speed 1s0)
     (:large-asteroid "asteroid_rgb.png"
		      :radius 4 :mass 14 :speed 1s0)
     (:planetoid "rocky_planet_rgb.png"
		 :radius 8 :mass 28 :speed 1s0))
    ;; level 1
    ((:small-rocky-planet "rocky_planet_rgb.png"
			  :radius 16 :mass 400 :speed 1s0)
     (:medium-rocky-planet "rocky_planet_rgb.png"
			   :radius 32 :mass 800 :speed 1s0)
     (:gas-planet "jovian_rgb.png"
		  :radius 64 :mass 1600 :speed 1s0)
     (:gas-giant "jovian_rgb.png"
		 :radius 192 :mass 4800 :speed 1s0))

    ;; level 2
    ((:small-star "sun_rgb.png"
		  :radius 400 :mass 90000 :speed 1s0)
     (:large-star "sun_rgb.png"
		  :radius 600 :mass 180000 :speed 1s0))

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
    ((:tiny-asteroid "asteroid_rgb.png" 60
		     :radius 1 :mass 1
		     :speed (0 . 0.5)
		     :colors ((,(nrgb 84 63 41)
				,(nrgb 150 123 95)
				,(nrgb 255 199 140))
			      (,(nrgb 49 45 42)
				,(nrgb 95 91 86)
				,(nrgb 156 147 137))
			      (,(nrgb 41 62 84)
				,(nrgb 95 122 150)
				,(nrgb 140 196 255))))
     ;;(:medium-asteroid "" 5)
     (:comet "comet_rgb.png" 10
	     :radius 1.5 :mass 28
	     :rotation ,(/ +pi+ 2)
	     :speed (1.5 . 2)
	     :colors ((,(nrgb 9 83 145)
			,(nrgb 94 224 238)
			,(nrgb 210 255 253))
		      (,(nrgb 82 49 137)
			,(nrgb 207 105 222)
			,(nrgb 244 202 249))))

     (:planetoid "rocky_planet_rgb.png" 3
		 :radius 8 :mass 28 :speed 1s0
		 :colors ((,(nrgb 84 63 41)
			    ,(nrgb 150 123 95)
			    ,(nrgb 255 199 140))
			  (,(nrgb 49 45 42)
			    ,(nrgb 95 91 86)
			    ,(nrgb 156 147 137))
			  (,(nrgb 41 62 84)
			    ,(nrgb 95 122 150)
			    ,(nrgb 140 196 255)))))

    (;; level 1
     (:planetoid "rocky_planet_rgb.png" 30
		 :radius 8 :mass 28
		 :speed 0.5
		 :colors ((,(nrgb 39 61 55)
			    ,(nrgb 97 175 175)
			    ,(nrgb 183 233 96))
			  (,(nrgb 53 39 61)
			    ,(nrgb 170 97 175)
			    ,(nrgb 96 192 233))
			  (,(nrgb 61 48 39)
			    ,(nrgb 175 159 97)
			    ,(nrgb 219 181 33))))
     (:small-rocky-planet "rocky_planet_rgb.png" 30
			  :radius 16 :mass 40
			  :speed 0.5
			  :colors ((,(nrgb 39 61 55)
			    ,(nrgb 97 175 175)
			    ,(nrgb 183 233 96))
			  (,(nrgb 53 39 61)
			    ,(nrgb 170 97 175)
			    ,(nrgb 96 192 233))
			  (,(nrgb 61 48 39)
			    ,(nrgb 175 159 97)
			    ,(nrgb 219 181 33))))
     (:medium-rocky-planet "rocky_planet_rgb.png" 10
			   :radius 32 :mass 80
			   :speed 0.5
			   :colors ((,(nrgb 39 61 55)
			    ,(nrgb 97 175 175)
			    ,(nrgb 183 233 96))
			  (,(nrgb 53 39 61)
			    ,(nrgb 170 97 175)
			    ,(nrgb 96 192 233))
			  (,(nrgb 61 48 39)
			    ,(nrgb 175 159 97)
			    ,(nrgb 219 181 33))))
     (:gas-planet "jovian_rgb.png" 1
		  :radius 1.5 :mass 28
		  :speed 0.5
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
			     ,(nrgb 122 253 107)))))

    (;; level 2
     (:gas-planet "jovian_rgb.png" 1
		  :radius 1.5 :mass 28
		  :speed 0.5
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
			     ,(nrgb 122 253 107)))))))

(defun parse-speed (x)
  (rotate-v2
   (v! 0 (etypecase x
	   (cons (dbind (s . e) x
		   (+ s (random (float (- e s))))))
	   (number (+ (random (float x))))))
   (random (* 2 +pi+))))
