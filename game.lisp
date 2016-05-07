(in-package #:kepler)
(in-readtable fn:fn-reader)

(defconstant +fps+ 60s0) ;; frames per second
(defconstant +spf+ (/ 1s0 +fps+)) ;; seconds per frame
(defconstant +sps+ 60s0) ;; update steps per second
(defconstant +fts+ (/ 1s0 60s0)) ;; fixed time step (seconds per step)

(defvar *camera* nil)
(defvar *player* nil)
(defvar *rocks* nil)
(defvar *misc-draw* nil)
(defvar *blend* nil)
(defvar *sky-tex* nil)
(defvar *dust-tex* nil)
(defvar *sky-quad* nil)
(defvar *nebula-tex* nil)
(defvar *particle-system* nil)
(defvar *nebula-falloff* 20s0)
(defvar *eat-sound* nil)
(defvar *grow-sound* nil)
(defvar *shrink-sound* nil)
(defvar *collision-sound* nil)
(defvar *logo* nil)
(defvar *temporal-draw-funcs* (ttm:make-tfunc-pool))
(defvar *first-pass-fbo* nil)
(defvar *first-pass-sampler* nil)

(defun-g test ((x :vec2) &uniform (y (:vec2 10)))
  (aref y 0)
  (v! 0 0 0 0))

(defun init ()
  (unless *sky-tex*
    ;;  sounds
    (sdl2-mixer:init :ogg)
    (sdl2-mixer:open-audio 22050 :s16sys 1 1024)
    (sdl2-mixer:play-music (load-ogg "Psyonik_-_The_Heavens_Sing.ogg"))
    (sdl2-mixer:volume-music 15)
    (setf *eat-sound* (load-wav "eat.wav"))
    (setf *grow-sound* (load-wav "grow.wav"))
    (setf *shrink-sound* (load-wav "shrink.wav"))
    (setf *collision-sound* (load-wav "collision.wav"))

    ;; graphics
    (setf *camera* (make-camera))
    (setf *blend* (make-blending-params))
    (with-viewport (camera-viewport *camera*)
      (setf *first-pass-fbo* (make-fbo 0 :d))
      (setf *first-pass-sampler* (sample (attachment-tex *first-pass-fbo* 0))))

    ;; game entities
    (setf *sky-quad* (make-gpu-quad))
    (setf *player* (make-player))

    ;; particles
    (init-particles)
    (setf *particle-system* (make-particle-system))

    (populate-velocities-using-func
	 (lambda (ptr x y)
	   (declare (ignorable x y))
	   (setf (cffi:mem-aref ptr :float 0) (- (random 0.01) 0.005)
		 (cffi:mem-aref ptr :float 1) (- (random 0.01) 0.005)
		 (cffi:mem-aref ptr :float 2) 0s0)))

    ;; input
    (skitter:listen-to λ(mouse-listener _ _1) (skitter:mouse 0) :pos)
    (skitter:listen-to λ(window-size-callback _ _1) (skitter:window 0) :size)

    ;; media
    (setf *logo* (load-texture "logo.png"))
    (setf *dust-tex* (load-texture "star_02.png"))
    (setf *nebula-tex* (load-texture "nebula.jpg"))
    (setf *sky-tex* (load-texture "space_bg.png"))

    ;; and begin
    (reset-game 0 0)))

;;----------------------------------------------------------------------

(defun reset-game (&optional level stage)
  (let ((level (or level (game-state-level *game-state*)))
	(stage (or stage (game-state-stage *game-state*))))
    (setf *game-state* (make-game-state :level level :stage stage))
    (reset-player *player* level stage)
    (setup-level level)
    (when (= level stage 0)
      (ttm:add
       (tlambda ()
	 (then
	   (before (seconds 3)
	     (draw-quad *logo* %progress%))
	   (before (seconds 3)
	     (draw-quad *logo* 1s0))
	   (before (seconds 1)
	     (draw-quad *logo* (- 1s0 %progress%)))
	   (once (goto-next-stage *player* *game-state*))))
       *temporal-draw-funcs*))
    t))

(defun reset-player (&optional (player *player*) level stage)
  (let ((level (or level (game-state-level *game-state*)))
	(stage (or stage (game-state-stage *game-state*))))
    (setf (actoroid-invincible-for-seconds player) 3s0)
    (setf (player-stuck player) nil)
    (setf (actoroid-position player) (v! 0 0)
	  (actoroid-velocity player) (v! 0 0))
    (update-player-data player level stage)))

(defun player-ready-for-next-stage-p (player game-state)
  (let ((next-stage (calc-next-stage (game-state-level game-state)
				     (game-state-stage game-state))))
    (when next-stage
      (dbind (&key mass &allow-other-keys) (apply #'player-stats next-stage)
	(>= (mass player) mass)))))

(defun update-player-data (player level stage)
  (dbind (_ tex &key mass speed flare &allow-other-keys) (player-stats level stage)
    (declare (ignore _))
    (setf (player-texture player) (load-texture tex))
    (setf (player-max-speed player) speed)
    (setf (actoroid-mass player) mass)
    (setf (actoroid-colors player)
	  (get-new-colors (mapcar #'car (player-stuck player))))
    ;; flare
    (setf (actoroid-flare player)
	  (loop :for f :in flare :collect
	     (dbind (tex ratio &key (at-back t) (rotation-speed 0s0)) f
	       (make-instance
		'flare
		:tex (load-texture tex)
		:ratio ratio
		:at-back at-back
		:rotation-speed rotation-speed))))
    ;; these two fire off temporal lambdas
    (resize-player level stage)
    (when (player-stuck player) (remove-rocks player))
    ;; done
    player))

(defun remove-rocks (player)
  (let* ((rocks (mapcar #'car (player-stuck player)))
	 (offsets (mapcar λ(v2:- (actoroid-position _)
				 (actoroid-position player))
			  rocks)))
    (push (mapcar #'rock->spec rocks)
	  (player-collected player))
    (setf *misc-draw* (append rocks *misc-draw*))
    (setf (player-stuck player) nil)
    (incf (actoroid-mass player) (reduce #'+ (mapcar #'mass rocks)))
    (ttm:add
     (tlambda ()
       (then
	 (before (seconds 0.4)
	   (loop :for r :in rocks :for o :in offsets :do
	      (setf (actoroid-position r)
		    (v2:+ (actoroid-position player)
			  (v2:*s o (- 1s0 (easing-f:in-quad %progress%)))))))
	 (once
	  (setf *misc-draw*
		(reduce λ(remove _1 _) rocks :initial-value *misc-draw*))))))))

(defun get-new-colors (rocks)
  (labels ((c= (x y) (every #'v:= x y)))
    (let* ((colors (mapcar #'actoroid-colors rocks))
	   (deduped (remove-duplicates colors :test #'c=))
	   ;; pair up the counts and colors
	   (pairs (mapcar #'cons deduped
			  (mapcar (lambda (x) (count-if λ(c= x _) colors))
				  deduped)))
	   (sorted (sort pairs #'> :key #'cdr))
	   (final-colors (mapcar #'car (subseq sorted 0 (min 3 (length sorted))))))
      (make-array 3 :initial-contents
		  (case= (length final-colors)
		    (0 (list (nrgb 100 100 100)
			     (nrgb 130 130 130)
			     (nrgb 200 200 200)))
		    (1 (list (elt (first final-colors) 0)
			     (elt (first final-colors) 1)
			     (elt (first final-colors) 2)))
		    (2 (list (elt (first final-colors) 0)
			     (elt (first final-colors) 1)
			     (elt (second final-colors) 2)))
		    (3 (list (elt (first final-colors) 0)
			     (elt (second final-colors) 1)
			     (elt (third final-colors) 2))))))))

;;----------------------------------------------------------------------

(defun calc-next-stage (level stage)
  (vbind (level-inc stage)
      (floor (1+ stage)
	     (length (elt *player-journey* level)))
    (let ((level (+ level level-inc)))
      (unless (>= level (length *player-journey*))
	(list level stage)))))

(defun calc-last-stage (level stage)
  (vbind (level-inc stage)
      (floor (1- stage)
	     (length (elt *player-journey* level)))
    (let ((level (+ level level-inc)))
      (if (< level 0)
	  '(0 0)
	  (list level stage)))))

(defun setup-level (level)
  (declare (optimize debug))
  ;; {TODO} do something with field size
  (dbind (ptex psize palpha spread pcol) (passive-particle-spec level)
    (setf *dust-tex* (load-texture ptex)
	  (particle-system-neg-alpha *particle-system*) palpha
	  (particle-system-colors *particle-system*) (make-array 3 :initial-contents pcol)
	  (particle-system-size *particle-system*) psize
	  (particle-system-spread *particle-system*) spread))
  ;; add new bodies from spec
  ;; {TODO} for now just dump *rocks*, later animate this
  (setf *rocks*
	(loop :for (count spec) :in (stage-bodies-spec level) :append
	   (loop :for i :below count :collect (spec->rock level spec)))))

(defun spec->rock (level spec)
  (let ((space-field-size (elt *space-field-sizes* level))
	(rand-rotation (random (* +pi+ 2))))
    (dbind (name tex &key radius mass colors speed rotation
		 flare flare-chance-in-%) spec
      (make-actoroid
       :kind name
       :texture (if (sampler-p tex)
		    tex
		    (load-texture tex))
       :colors (when colors (make-array
			     3 :initial-contents
			     (alexandria:random-elt colors)))
       :position (calc-starting-pos radius space-field-size
				    level 0)
       :velocity (rotate-v2
		  (v! 0 (parse-speed speed))
		  rand-rotation)
       :rotation (- (or rotation 0s0) rand-rotation)
       :mass mass
       :radius radius
       :flare (when (and flare (< (random 100s0) flare-chance-in-%))
		(loop :for f :in flare :collect
		   (dbind (tex ratio &key (at-back t)
			       (rot-speed 0s0)) f
		     (make-instance
		      'flare
		      :tex (load-texture tex)
		      :ratio ratio
		      :at-back at-back
		      :rotation-speed rot-speed))))))))

(defun rock->spec (rock)
  (let ((speed (* (v2:length (actoroid-velocity rock)) 2))
	(flare (actoroid-flare rock)))
    (list (actoroid-kind rock)
	  (actoroid-texture rock)
	  :radius (actoroid-radius rock)
	  :mass (actoroid-mass rock)
	  :rotation (actoroid-rotation rock)
	  :speed (cons speed speed)
	  :colors (list (map 'list #'identity
			     (actoroid-colors (first *rocks*))))
	  :flare (when flare
		   (loop :for f :in flare :collect
		      (with-slots (tex ratio at-back-p rotatation-speed) f
			(list tex ratio :at-back at-back-p
			      :rotation-speed rotatation-speed))))
	  :flare-chance-in-% (when flare 100s0))))

(defun calc-starting-pos (rock-radius space-field-size level stage)
  (dbind (_ _1 &key radius &allow-other-keys) (player-stats level stage)
    (declare (ignore _ _1))
    (let* ((min (* (+ rock-radius radius) 4))
	   (range (- (float space-field-size) min)))
      (rotate-v2
       (v! 0 (+ min (random range)))
       (random (* +pi+ 2))))))

(defun goto-next-stage (player game-state)
  (let ((last-level (game-state-level game-state))
	(last-stage (game-state-stage game-state)))
    (sdl2-mixer:play-channel -1 *grow-sound* 0)
    (dbind (level stage) (calc-next-stage last-level last-stage)
      ;; update the game state
      (setf (game-state-level game-state) level
	    (game-state-stage game-state) stage)
      ;; update player
      (update-player-data player level stage)
      ;; check if we need to do fancy transition
      (if (= stage 0)
	  (ttm:add
	   (tlambda ()
	     (then
	       (once (print "*Fancy Level Transition*"))
	       (once (print "- ooh wasnt that nice -"))
	       (once (setup-level level)))))
	  (ttm:add
	   (tlambda ()
	     (then
	       (once (print "*Fancy Stage Transition*"))
	       (once (print "- ooh wasnt that nice -")))))))))

(defun maybe-goto-next-stage (player game-state)
  (let ((ready (player-ready-for-next-stage-p player game-state)))
    ;;(break "~a ~a ~a" player (mass player) ready)
    (if ready
	(goto-next-stage player game-state)
	(sdl2-mixer:play-channel -1 *eat-sound* 0))))

(defun go-back-a-stage (player game-state)
  (let ((last-level (game-state-level game-state))
	(last-stage (game-state-stage game-state)))
    (unless (= last-level last-stage 0)
      (sdl2-mixer:play-channel -1 *shrink-sound* 0)
      (dbind (level stage) (calc-last-stage last-level last-stage)
	;; update the game state
	(setf (game-state-level game-state) level
	      (game-state-stage game-state) stage)
	;; update player
	(update-player-data player level stage)
	;;
	(pop-collected-rocks player level)
	;; check if we need to do fancy transition
	(when (< level last-level)
	  (ttm:add
	   (tlambda ()
	     (then
	       (once (print "*Aww going back transition*"))
	       (once (print "- okidokey -"))
	       (once (setup-level level))))))))))

(defun pop-collected-rocks (player level)
  (let ((released (mapcar λ(spec->rock level _)
			  (pop (player-collected player)))))
    (loop :for r :in released :do
       (setf (actoroid-position r) (actoroid-position player)
	     (actoroid-invincible-for-seconds r) 2s0))
    (setf *rocks* (append released *rocks*))))

(defun bump (player)
  (when (<= (actoroid-invincible-for-seconds player) 0s0)
    (sdl2-mixer:play-channel -1 *collision-sound* 0)
    (shake-cam)
    (setf (actoroid-invincible-for-seconds player) 0.5)))

(defparameter *game-state* (make-game-state))

(defun resize-player (level stage)
  (dbind (_ _1 &key radius &allow-other-keys) (player-stats level stage)
    (declare (ignore _ _1))
    (ttm:add
     (let* ((start-size (actoroid-radius *player*))
	    (start-zoom (zoom *camera*))
	    (size-change (- radius start-size))
	    (zoom-change (- (get-zoom-for-stage level stage)
			    start-zoom)))
       (tlambda ()
	 (then
	   (before (seconds 1)
	     (setf (actoroid-radius *player*)
		   (+ start-size
		      (* size-change (easing-f:out-bounce %progress%)))))
	   (before (seconds 1)
	     (setf (zoom *camera*)
		   (+ start-zoom
		      (* zoom-change (easing-f:out-bounce %progress%)))))))))))


;;----------------------------------------------------------------------

(defun-g actor-vert ((vert g-pt) &uniform (pos :vec2) (rot :mat3)
		     (cam cam-g :ubo) (rad :float) (ymod :float))
  (let* ((vpos (* rot (* (pos vert) rad))))
    (values (+ (v! (cam-it (+ vpos (v! pos 0))
			   cam)
		   1)
	       (v! 0 0 ymod 0))
	    (tex vert))))

(defun-g actor-replace-frag ((tc :vec2) &uniform (tex :sampler-2d)
			     (rcol :vec3) (gcol :vec3) (bcol :vec3)
			     (field-size :float) (falloff :float) (pos :vec2))
  (let ((map (texture tex tc))
	(f (/ (min falloff (max 0s0 (- (length pos) (- field-size falloff)))) falloff)))
    (v! (+ (* rcol (v:x map))
	   (* gcol (v:y map))
	   (* bcol (v:z map)))
	(* (v:w map) (- 1 f)))))

(defun-g actor-replace-stuck ((tc :vec2) &uniform (tex :sampler-2d)
			      (rcol :vec3) (gcol :vec3) (bcol :vec3)
			      (neg-alpha :float))
  (let ((map (texture tex tc)))
    (v! (+ (* rcol (v:x map))
	   (* gcol (v:y map))
	   (* bcol (v:z map)))
	(- (v:w map) neg-alpha))))

(def-g-> actor-replace-color-pipeline ()
  #'actor-vert #'actor-replace-frag)

(def-g-> actor-replace-color-pipeline2 ()
  #'actor-vert #'actor-replace-stuck)

(defun draw-actor (x &optional (ymod 0s0))
  (declare (optimize debug))
  (map-g #'actor-replace-color-pipeline
	 (actoroid-stream x)
	 :ymod ymod
	 :pos (actoroid-position x)
	 :tex (actoroid-texture x)
	 :rot (m3:rotation-z (actoroid-rotation x))
	 :cam (camera-ubo *camera*)
	 :rad (actoroid-radius x)
	 :rcol (aref (actoroid-colors x) 0)
	 :gcol (aref (actoroid-colors x) 1)
	 :bcol (aref (actoroid-colors x) 2)
	 :field-size (field-size)
	 :falloff *nebula-falloff*)
  (draw-flare x ymod))

(defun draw-flare (x &optional (ymod 0s0))
  (loop :for flare :in (actoroid-flare x) :do
    (with-slots (tex ratio at-back-p rot rotatation-speed) flare
      (map-g #'actor-replace-color-pipeline
	     (actoroid-stream x)
	     :ymod (+ ymod (if at-back-p 0s0 -0.0001))
	     :pos (v2:- (actoroid-position x) (v! 0 0))
	     :tex tex
	     :rot (m3:rotation-z rot)
	     :cam (camera-ubo *camera*)
	     :rad (* (actoroid-radius x) ratio)
	     :rcol (aref (actoroid-colors x) 0)
	     :gcol (aref (actoroid-colors x) 1)
	     :bcol (aref (actoroid-colors x) 2)
	     :field-size (field-size)
	     :falloff *nebula-falloff*))))

(defun draw-player (x)
  (declare (optimize debug))
  (map-g #'actor-replace-color-pipeline2
	 (actoroid-stream x)
	 :ymod 0.1
	 :pos (actoroid-position x)
	 :tex (actoroid-texture x)
	 :rot (m3:rotation-z 0s0)
	 :cam (camera-ubo *camera*)
	 :rad (actoroid-radius x)
	 :rcol (aref (actoroid-colors x) 0)
	 :gcol (aref (actoroid-colors x) 1)
	 :bcol (aref (actoroid-colors x) 2)
	 :neg-alpha (if (> (actoroid-invincible-for-seconds x) 0.0)
			(+ 0.5 (/ (sin (* 30 (actoroid-invincible-for-seconds x))) 2))
			0s0))
  (draw-flare x))

(defun draw-stuck (x &optional (ymod 0s0))
  (declare (optimize debug))
  (map-g #'actor-replace-color-pipeline2
	 (actoroid-stream x)
	 :ymod ymod
	 :pos (actoroid-position x)
	 :tex (actoroid-texture x)
	 :rot (m3:rotation-z (actoroid-rotation x))
	 :cam (camera-ubo *camera*)
	 :rad (actoroid-radius x)
	 :rcol (aref (actoroid-colors x) 0)
	 :gcol (aref (actoroid-colors x) 1)
	 :bcol (aref (actoroid-colors x) 2)
	 :neg-alpha 0s0)
  (draw-flare x ymod))

(defun update-stuck ()
  (loop :for (s . offset) :in (player-stuck *player*) :do
     (setf (actoroid-position s)
	   (v2:+ (actoroid-position *player*) offset))))

;;----------------------------------------------------------------------

(defun-g sky-vert ((vert g-pt) &uniform (cam cam-g :ubo) (player-pos :vec2))
  (values (v! (s~ (pos vert) :xy) 0.99 1.0)
	  (tex vert)
	  (s~ (pos vert) :xy)))

(defun-g sky-frag ((tc :vec2) (pos :vec2) &uniform (tex :sampler-2d) (nebula :sampler-2d)
		   (cam cam-g :ubo) (field-size :float) (nebula-falloff :float))
  (let* ((screen-ratio (v! (/ (v:x (cam-g-size cam))
			      (v:y (cam-g-size cam)))
			   1))
	 (pixel-pos (+ (s~ (cam-g-position cam) :xy)
		       (* (* pos screen-ratio)
			  (cam-g-zoom cam))))
	 (dist (- (length pixel-pos)
		  field-size
		  nebula-falloff))
	 (factor (/ (min (max dist 0s0) nebula-falloff)
		    nebula-falloff)))
    (mix (* (texture tex tc) 0.65)
	 (texture nebula tc)
	 factor)))

(def-g-> sky-pipeline2 ()
  #'sky-vert #'sky-frag)

(defun draw-sky ()
  (map-g #'sky-pipeline2
	 *sky-quad*
	 :tex *sky-tex*
	 :nebula *nebula-tex*
	 :player-pos (actoroid-position *player*)
	 :cam (camera-ubo *camera*)
	 :field-size (field-size)
	 :nebula-falloff *nebula-falloff*))

(defun-g splat-vert ((vert g-pt) &uniform (ymult :float))
  (values (v! (s~ (pos vert) :xy) 0.999 1.0)
	  (* (tex vert) (v! 1 ymult))))

(defun-g splat-frag ((tc :vec2) &uniform (tex :sampler-2d) (alpha :float))
  (let ((col (texture tex tc)))
    (v! (s~ col :xyz)
	(* (v:w col) alpha))))

(def-g-> splat ()
  #'splat-vert #'splat-frag)

(defun draw-quad (tex &optional (alpha 1s0) (ymult 1s0))
  (map-g #'splat *sky-quad* :tex tex :alpha alpha :ymult ymult))

;;----------------------------------------------------------------------

(defun update-player (&optional (player *player*))
  (update-flare-for player)
  (setf (actoroid-position player)
	(v2:+ (actoroid-position player)
	      (v2:*s (actoroid-velocity player) +fts+)))
  (decf (actoroid-invincible-for-seconds player) +fts+)
  (setf (actoroid-velocity player)
	(v2:- (actoroid-velocity player)
	      (v2:*s (actoroid-velocity player) (* 0.95 +fts+))))
  (when (skitter:key-down-p key.escape)
    (reset-game 0 0))
  ;; accelerate when mouse down
  (if (skitter:mouse-down-p mouse.left)
      ;; accelerate
      (let* ((target-vel (rotate-v2 (v! 0 (player-max-speed player))
				    (actoroid-rotation player)))
	     (ease (easing-f:out-cubic
		    (setf (player-accel-ramp player)
			  (min 1s0 (+ (player-accel-ramp player)
				      (* 4 +fts+))))))
	     (old-vel (player-key-up-vel player)))
	(setf (player-decel-ramp player) 1s0)
	(setf (player-key-down-vel player)
	      (setf (actoroid-velocity player)
		    (v! (lerp (x old-vel) (x target-vel) ease)
			(lerp (y old-vel) (y target-vel) ease)))))
      ;; decelerate
      (let* ((ease (easing-f:in-cubic
		    (setf (player-decel-ramp player)
			  (max 0s0 (- (player-decel-ramp player)
				      +fts+)))))
	     (old-vel (player-key-down-vel player)))
	(setf (player-accel-ramp player) 0s0)
	(setf (player-key-up-vel player)
	      (setf (actoroid-velocity player)
		    (v2:*s old-vel ease)))))
  (nebula-push-back player)
  (update-stuck)
  (check-for-player-collisions player))

(defun actor-offset (a b)
  (v2:- (actoroid-position b)
	(actoroid-position a)))

(defun actors-colliding-p (a b)
  (let ((o (actor-offset a b)))
    (<= (v2:length o)
	(+ (actoroid-radius a)
	   (actoroid-radius b)))))

(defun check-for-player-collisions (player)
  (when (<= (actoroid-invincible-for-seconds player) 0s0)
    (symbol-macrolet ((stuck (player-stuck player)))
      (loop :for a :in (cons player (mapcar #'car stuck)) :do
	 (let ((col-with (find-if λ(actors-colliding-p _ a) *rocks*)))
	   (when col-with
	     (if (<= (mass col-with) (mass player))
		 (unless (> (actoroid-invincible-for-seconds col-with) 0s0)
		   (attach-rock-to-player player col-with a))
		 (progn
		   (bump player)
		   (if (null stuck)
		       (go-back-a-stage player *game-state*)
		       (unless (eq a player)
			 (detach-rock-from-player player a)))))))))))

(defun attach-rock-to-player (player rock stick-to)
  (symbol-macrolet ((stuck (player-stuck player)))
    (let ((o (actor-offset player rock)))
      (setf *rocks* (remove rock *rocks*))
      (push (cons rock (v2:- o
			     (v2:*s (actor-offset stick-to rock) 0.05)
			     (v2:*s (actor-offset player rock) 0.05)))
	    stuck))
    (maybe-goto-next-stage player *game-state*)))

(defun detach-rock-from-player (player rock)
  (symbol-macrolet ((stuck (player-stuck player))
		    (vel (actoroid-velocity rock)))
    (let ((o (actor-offset player rock)))
      (setf (actoroid-invincible-for-seconds rock) 1s0)
      (setf stuck (remove rock stuck :key #'car)
	    vel (v2:*s (v2:normalize (v! (y o) (x o))) 0.3))
      (push rock *rocks*)))
  (remove-any-seperate-rocks))

(defun remove-any-seperate-rocks ()
  (let* ((stuck (mapcar #'car (player-stuck *player*)))
	 (attached (make-array (length stuck) :initial-contents
			       (mapcar λ(actors-colliding-p _ *player*) stuck)))
	 (neighbours
	  (loop :for s :in stuck :collect
	     (loop :for n :in stuck :for i :from 0
		:when (and (not (eq s n)) (actors-colliding-p s n))
		:collect i))))
    (loop :until (every #'identity attached) :for d :from 0 :do
       (when (> d 500)
	 (loop :for i :below (length attached) :do
	    (setf (aref attached i) :f)))
       (loop :for s :in stuck :for ns :in neighbours :for i :from 0 :do
	  (unless (aref attached i)
	    (setf (aref attached i)
		  (cond ((null ns) :f)
			((some λ(eq t (aref attached _)) ns) t)
			((every λ(eq :f (aref attached _)) ns) :f)
			(t nil))))))
    (loop :for s :in stuck :for a :across attached :do
       (when (eq :f a)
	 (detach-rock-from-player *player* s)))))

(defun nebula-push-back (player)
  (let ((ppos (actoroid-position player)))
    (setf (actoroid-velocity player)
	  (v2:+ (actoroid-velocity player)
		(v2:*s (v2:normalize ppos)
		       (* (- (max 0s0 (- (v2:length ppos) (field-size))))
			  300s0
			  +fts+))))))

(defvar fff 0)
(defparameter sss (make-stepper (seconds 1)))
(defun draw ()
  ;; (incf fff)
  ;; (when (funcall sss)
  ;;   (print fff)
  ;;   (setf fff 0))
  (with-viewport (camera-viewport *camera*)
    (with-blending *blend*
      (with-fbo-bound (*first-pass-fbo* :with-blending nil :with-viewport nil)
	(clear)

	(ttm:update *temporal-draw-funcs*)
	(draw-sky)
	(draw-passive-particles *particle-system* *camera* *dust-tex*)

	(when *rocks*
	  (let* ((min 0.3) (max 0.4) (range (- max min))
		 (mult (/ range (length *rocks*))))
	    (loop :for a :in *rocks* :for i :from 0 :do
	       (draw-actor a (- max (* i mult)))))
	  ;;
	  (let* ((min 0.4) (max 0.5) (range (- max min))
		 (mult (/ range (length *rocks*))))
	    (loop :for s :in (player-stuck *player*) :for i :from 0 :do
	       (draw-stuck (car s) (- max (* i mult))))))

	(loop :for a :in *misc-draw* :do (draw-actor a))

	(draw-player *player*)))
    (clear)
    (bloom (get-gpu-quad) *first-pass-sampler* (abs (sin (* (get-internal-real-time) 0.001))))

    ;;(draw-quad *first-pass-sampler* 1s0 -1s0)
    (swap)))

(defun update-rocks ()
  (let ((field-size (field-size)))
    (loop :for r :in *rocks* :do
       (update-flare-for r)
       (symbol-macrolet ((pos (actoroid-position r))
			 (vel (actoroid-velocity r)))
	 (setf pos (v2:+ pos (v2:*s vel +fts+)))
	 (decf (actoroid-invincible-for-seconds r) +fts+)
	 (when (> (v2:length pos) field-size)
	   (setf pos (v2:*s (v2:normalize pos) (- (- field-size 0.1s0)))))))))

(defun update-flare-for (x)
  (let ((flares (actoroid-flare x)))
    (when flares
      (loop :for f :in flares :do
	 (with-slots (rot rotatation-speed) f
	   (incf rot (* rotatation-speed +fts+)))))))

(defun update ()
  (setf (cam-pos *camera*) (actoroid-position *player*))
  (update-player)
  (update-rocks)
  (update-particles *particle-system*)
  (ttm:update))

(let ((running nil))
  (defun run-kepler (&optional for-frames force-stepper)
    (assert (or (null for-frames) (numberp for-frames)))
    (unwind-protect
	 (progn (format t "-kepler started-~%")
		(setf running t)
		(let ((game-stepper (temporal-functions:make-stepper
				     (seconds +spf+)))
		      (repl-stepper (temporal-functions:make-stepper
				     (seconds (/ 1.0 10.0)))))
		  (init)
		  (loop :while (and running
				    (not (shutting-down-p))
				    (if for-frames (>= (decf for-frames) 0) t))
		     :do (continuable
			   (cepl:step-host)
			   (when (or force-stepper (funcall game-stepper))
			     (update)
			     (draw))
			   (when (or force-stepper (funcall repl-stepper))
			     (update-repl-link))))))
      (setf running nil)
      (format t "-kepler stopped-~%"))
    t)
  (defun stop-kepler ()
    (sdl2-mixer:halt-music)
    (setf running nil)))

;;----------------------------------------------------------------------

(defun window-size-listener (event timestamp)
  (format t "Window event ~s at ~s" event timestamp))

(let ((last-timestamp 0))
  (defun mouse-listener (event timestamp)
    (setf last-timestamp timestamp)
    (let* ((d (skitter:xy-pos-vec event))
	   (res (viewport-resolution (camera-viewport *camera*)))
	   (v (v2:normalize (v! (- (v:x d) (/ (v:x res) 2s0))
				(- (- (v:y d) (/ (v:y res) 2s0))))))
	   (a (acos (v2:dot (v! 0 1) v)))
	   (a (if (< (x v) 0)
		  (- a)
		  a)))
      (setf (actoroid-rotation *player*) (- a)))))

;;----------------------------------------------------------------------

(defun shake-cam (&optional (camera *camera*))
  (let ((zoom (zoom camera)))
    (ttm:add
     (tlambda ()
       (before (seconds 0.5)
	 (setf (cam-pos camera)
	       (v2:+ (cam-pos camera)
		     (v2:*s (v! (* zoom 0.04 (sin (* 10 %progress%)))
				(* zoom 0.04 (cos (* 20 %progress%))))
			    (- 1s0 (easing-f:in-quad %progress%))))))))))

;;----------------------------------------------------------------------

(defun reshape (new-dimensions)
  (let ((new-dimensions (v! (v:x new-dimensions) (v:y new-dimensions))))
    (print new-dimensions)
    (update-viewport-size *camera* (v! new-dimensions))
    (free *first-pass-fbo*)
    (with-viewport (camera-viewport *camera*)
      (setf *first-pass-fbo* (make-fbo 0 :d))
      (setf *first-pass-sampler* (sample (attachment-tex *first-pass-fbo* 0))))))

(defun window-size-callback (event timestamp)
  (declare (ignore timestamp))
  (reshape (skitter:size-2d-vec event)))
