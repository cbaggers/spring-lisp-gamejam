(in-package #:kepler)
(in-readtable fn:fn-reader)

(defvar *camera* nil)
(defvar *player* nil)
(defvar *rocks* nil)
(defvar *misc-draw* nil)
(defvar *blend* nil)
(defvar *sky-tex* nil)
(defvar *moon-tex* nil)
(defvar *sky-quad* nil)

(defun init ()
  (skitter:listen-to (lambda (x y) (mouse-listener x y)) (skitter:mouse 0) :pos)
  (setf *sky-tex* (load-texture "tempSky.jpg"))
  (setf *moon-tex* (load-texture "temp.png"))
  (setf *sky-quad* (make-gpu-quad))
  (setf *camera* (make-camera))
  (setf *player* (make-player))
  (setf *blend* (make-blending-params))
  (reset-player))

;;----------------------------------------------------------------------

(defun make-player ()
  (dbind (name tex &key radius mass) (player-stats 0 0)
    (declare (ignore name))
    (%make-player
     :texture (load-texture tex)
     :radius radius
     :mass mass)))

(defun reset-game (&optional level stage)
  (let ((level (or level (game-state-level *game-state*)))
	(stage (or stage (game-state-stage *game-state*))))
    (setf *game-state* (make-game-state :level level :stage stage))
    (reset-player *player* level stage)
    (setup-level level)
    t))

(defun reset-player (&optional (player *player*) level stage)
  (let ((level (or level (game-state-level *game-state*)))
	(stage (or stage (game-state-stage *game-state*))))
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
  (dbind (_ tex &key mass &allow-other-keys) (player-stats level stage)
    (declare (ignore _))
    (setf (player-texture player) (load-texture tex))
    (setf (actoroid-mass player) mass)
    (setf (actoroid-colors player)
	  (get-new-colors (mapcar #'car (player-stuck player))))
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
  (let ((space-field-size (elt *space-field-sizes* level)))
    ;; add new bodies from spec
    ;; {TODO} for now just dump *rocks*, later animate this
    (setf *rocks*
	  (loop :for spec :in (stage-bodies-spec level) :append
	     (dbind (name tex count &key radius mass colors speed rotation) spec
	       (loop :for i :below count :collect
		  (make-actoroid
		   :kind name
		   :texture (load-texture tex)
		   :colors (when colors (make-array
					 3 :initial-contents
					 (alexandria:random-elt colors)))
		   :position (rotate-v2
			      (v! 0 (random (float space-field-size)))
			      (random (* +pi+ 2)))
		   :velocity (rotate-v2
			      (v! 0 (parse-speed speed))
			      (random (* +pi+ 2)))
		   :rotation (or rotation 0s0)
		   :mass mass
		   :radius radius)))))))

(defun goto-next-stage (player game-state)
  (let ((last-level (game-state-level game-state))
	(last-stage (game-state-stage game-state)))
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
    (when ready
      (goto-next-stage player game-state))))

(defun go-back-a-stage (player game-state)
  (let ((last-level (game-state-level game-state))
	(last-stage (game-state-stage game-state)))
    (if (= last-level last-stage 0)
	(bump player)
	(dbind (level stage) (calc-last-stage last-level last-stage)
	  ;; update the game state
	  (setf (game-state-level game-state) level
		(game-state-stage game-state) stage)
	  ;; update player
	  (update-player-data player level stage)
	  ;; check if we need to do fancy transition
	  (when (< level last-level)
	    (ttm:add
	     (tlambda ()
	       (then
		 (once (print "*Aww going back transition*"))
		 (once (print "- okidokey -"))
		 (once (setup-level level))))))))))

(defun bump (player)
  (when (<= (actoroid-invincible-for-seconds player) 0s0)
    (print "*bump*")
    (setf (actoroid-invincible-for-seconds player) 3s0)))

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

(defmacro-g cam-it (pos cam)
  `(- (* ,pos
	 (v! (/ (/ (v:y (cam-g-size ,cam)) (v:x (cam-g-size ,cam)))
		(cam-g-zoom ,cam))
	     (/ 1 (cam-g-zoom ,cam))
	     1
	     1))
      (v! (cam-g-position ,cam) 0 0)))

;;----------------------------------------------------------------------

(defun-g actor-vert ((vert g-pt) &uniform (pos :vec2) (rot :mat3)
		      (cam cam-g :ubo) (rad :float))
  (let* ((vpos (* rot (* (pos vert) rad))))
    (values (cam-it (+ (v! vpos 1)
		       (v! pos 0 0))
		    cam)
	    (tex vert))))

(defun-g actor-frag ((tc :vec2) &uniform (tex :sampler-2d) (tint :vec3))
  (+ (texture tex tc) (v! tint 0)))

(defun-g actor-replace-frag ((tc :vec2) &uniform (tex :sampler-2d)
			     (rcol :vec3) (gcol :vec3) (bcol :vec3))
  (let ((map (texture tex tc)))
    (v! (+ (* rcol (v:x map))
	   (* gcol (v:y map))
	   (* bcol (v:z map)))
	(v:w map))))

(def-g-> actor-replace-color-pipeline ()
  #'actor-vert #'actor-replace-frag)

(defun draw-actor (x)
  (declare (optimize debug))
  (map-g #'actor-replace-color-pipeline
	 (actoroid-stream x)
	 :pos (actoroid-position x)
	 :tex (actoroid-texture x)
	 :rot (m3:rotation-z (actoroid-rotation x))
	 :cam (camera-ubo *camera*)
	 :rad (actoroid-radius x)
	 :rcol (aref (actoroid-colors x) 0)
	 :gcol (aref (actoroid-colors x) 1)
	 :bcol (aref (actoroid-colors x) 2)))

(defun draw-stuck (x)
  (declare (optimize debug))
  (map-g #'actor-replace-color-pipeline
	 (actoroid-stream x)
	 :pos (actoroid-position x)
	 :tex (actoroid-texture x)
	 :rot (m3:rotation-z (+ (actoroid-rotation x)
				(actoroid-rotation *player*)))
	 :cam (camera-ubo *camera*)
	 :rad (actoroid-radius x)
	 :rcol (aref (actoroid-colors x) 0)
	 :gcol (aref (actoroid-colors x) 1)
	 :bcol (aref (actoroid-colors x) 2)))

(defun update-stuck ()
  (loop :for (s . offset) :in (player-stuck *player*) :do
     (setf (actoroid-position s)
	   (v2:+ (actoroid-position *player*)
		 (rotate-v2 offset (actoroid-rotation *player*))))))

;;----------------------------------------------------------------------

(defun-g sky-vert ((vert g-pt) &uniform (cam cam-g :ubo) (player-pos :vec2))
  (let ((zoom-factor (- (cam-g-zoom cam) 8)))
    (values (v! (s~ (pos vert) :xy) 0.1 1.0)
	    (- (* (tex vert) zoom-factor)
	       (* (v! 0.5 0.5) zoom-factor)))))

(defun-g sky-frag ((tc :vec2) &uniform (tex :sampler-2d) (cam cam-g :ubo))
  (texture tex tc))

(def-g-> sky-pipeline ()
  #'sky-vert #'sky-frag)

(defun draw-sky ()
  (map-g #'sky-pipeline
	 *sky-quad*
	 :tex *sky-tex*
	 :player-pos (actoroid-position *player*)
	 :cam (camera-ubo *camera*) ))

;;----------------------------------------------------------------------

(defun update-player ()
  (setf (actoroid-position *player*)
	(v2:+ (actoroid-position *player*)
	      (actoroid-velocity *player*)
	      ;;(v2:/s (actoroid-velocity *player*) 60s0)
	      ))
  (decf (actoroid-invincible-for-seconds *player*) (/ 1s0 60s0))
  (setf (actoroid-velocity *player*)
	(v2:- (actoroid-velocity *player*)
	      (v2:*s (actoroid-velocity *player*) (/ 0.95 60s0))))
  (let ((v (rotate-v2 (v! 0 (player-max-speed *player*))
		      (actoroid-rotation *player*))))
    (if (skitter:mouse-down-p mouse.left)
	(setf (player-velocity-ramp-pos *player*)
	      (min 1s0 (+ (player-velocity-ramp-pos *player*) (/ 4 60s0))))
	(setf (player-velocity-ramp-pos *player*)
	      (max 0s0 (- (player-velocity-ramp-pos *player*) (/ 1 60s0)))))
    ;; update the velocity
    (setf (actoroid-velocity *player*)
	  (v2:*s v (easing-f:in-quint (player-velocity-ramp-pos *player*)))))
  (update-stuck)
  (check-for-player-collisions *player*))

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
		 (if (null stuck)
		     (go-back-a-stage player *game-state*)
		     (unless (eq a player)
		       (detach-rock-from-player player a))))))))))

(defun attach-rock-to-player (player rock stick-to)
  (symbol-macrolet ((stuck (player-stuck player)))
    (let ((o (actor-offset player rock)))
      (setf *rocks* (remove rock *rocks*))
      (push (cons rock (rotate-v2
			(v2:- o
			      (v2:*s (actor-offset stick-to rock) 0.2)
			      (v2:*s (actor-offset player rock) 0.2))
			(- (actoroid-rotation player))))
	    stuck))
    (maybe-goto-next-stage player *game-state*)))

(defun detach-rock-from-player (player rock)
  (symbol-macrolet ((stuck (player-stuck player))
		    (vel (actoroid-velocity rock)))
    (let ((o (actor-offset player rock)))
      (setf (actoroid-invincible-for-seconds rock) 2s0)
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
	 (break "oh ~a ~s ~s" attached neighbours stuck))
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


(defun draw ()
  (clear)
  (with-blending *blend*
    (with-viewport (camera-viewport *camera*)
      (draw-sky)
      (draw-actor *player*)
      (loop :for a :in *rocks* :do (draw-actor a))
      (loop :for a :in *misc-draw* :do (draw-actor a))
      (loop :for s :in (player-stuck *player*) :do (draw-stuck (car s)))))
  (swap))

(defun update-rocks ()
  (loop :for r :in *rocks* :do
     (symbol-macrolet ((pos (actoroid-position r))
		       (vel (actoroid-velocity r)))
       (setf pos (v2:+ pos vel))
       (decf (actoroid-invincible-for-seconds r)
	     (/ 1s0 60s0))
       (when (> (v2:length pos) 40.0)
	 (setf pos (v2:*s (v2:normalize pos) -39.9))))))

(defun update ()
  (setf (cam-pos *camera*) (v2:/s (actoroid-position *player*)
				  (zoom *camera*)))
  (update-player)
  (update-rocks)
  (ttm:update))

(let ((running nil))
  (defun run-kepler (&optional for-frames force-stepper)
    (assert (or (null for-frames) (numberp for-frames)))
    (unwind-protect
	 (progn (format t "-kepler started-~%")
		(setf running t)
		(let ((game-stepper (temporal-functions:make-stepper
				     (seconds (/ 1.0 60.0))))
		      (repl-stepper (temporal-functions:make-stepper
				     (seconds (/ 1.0 10.0)))))
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
  (defun stop-kepler () (setf running nil)))

;;----------------------------------------------------------------------

(defun window-size-listener (event timestamp)
  (format t "Window event ~s at ~s" event timestamp))

(let ((last-timestamp 0))
  (defun mouse-listener (event timestamp)
    (setf last-timestamp timestamp)
    (let* ((d (skitter:xy-pos-vec event))
	   (v (v2:normalize (v! (- (v:x d) 400)
				(- (- (v:y d) 300)))))
	   (a (acos (v2:dot (v! 0 1) v)))
	   (a (if (< (x v) 0)
		  (- a)
		  a)))
      (setf (actoroid-rotation *player*) (- a)))))

;;----------------------------------------------------------------------
