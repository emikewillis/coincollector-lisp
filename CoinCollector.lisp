;;; Libraries
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

;;; Macros
(defmacro with-edges-of (e1 e2 &rest rest)
  "Calculates the edges of 2 entities"
  `(let ((topA (second (pos ,e1)))
	 (bottomA (+ (second (pos ,e1)) (second (size ,e1))))
	 (leftA (first (pos ,e1)))
	 (rightA (+ (first (pos ,e1)) (first (size ,e1))))
	 (topB (second (pos ,e2)))
	 (bottomB (+ (second (pos ,e2)) (second (size ,e2))))
	 (leftB (first (pos ,e2)))
	 (rightB (+ (first (pos ,e2)) (first (size ,e2)))))
     ,@rest))

;;; Classes
(defclass entity ()
  ((pos :initarg :pos :initform '(0 0) :accessor pos)
   (size :initarg :size :initform '(32 32) :accessor size)
   (vel :initarg :vel :initform '(0 0) :accessor vel)
   (color :initarg :color :initform (sdl:color :r 255 :g 0 :b 255) :accessor color)
   (on-collision :initarg :on-collision :initform '() :accessor on-collision)
   (on-key-down :initarg :on-key-down :initform '() :accessor on-key-down)
   (on-key-up :initarg :on-key-up :initform '() :accessor on-key-up)))

(defclass coin (entity)
  ((pos :initform `( ,(random (- 640 16)) ,(random (- 480 16))))
   (size :initform '(16 16))
   (color :initform (sdl:color :r 255 :g 255 :b 0))))

(defclass player (entity)
  ((color :initform (sdl:color :r 255 :g 0 :b 0))
   (on-collision :initform (lambda (other)
			     (if (typep other 'coin)
				 (and (incf *score*) (move other))
				 nil)))
   (on-key-down :initform (lambda (instance key)
			    (when (sdl:key= key :sdl-key-right)
			      (setf (first (vel instance)) 2))
			    (when (sdl:key= key :sdl-key-left)
			      (setf (first (vel instance)) -2))
			    (when (sdl:key= key :sdl-key-down)
			      (setf (second (vel instance)) 2))
			    (when (sdl:key= key :sdl-key-up)
			      (setf (second (vel instance)) -2))))
   (on-key-up :initform (lambda (instance key)
			  (when (or (sdl:key= key :sdl-key-right) (sdl:key= key :sdl-key-left))
			    (setf (first (vel instance)) 0))
			  (when (or (sdl:key= key :sdl-key-down) (sdl:key= key :sdl-key-up))
			    (setf (second (vel instance)) 0))))))
			    


;; Methods
(defmethod check-entity-collision ((e1 entity) (e2 entity))
  (with-edges-of e1 e2
    (cond ((<= bottomA topB) nil)
	  ((>= topA bottomB) nil)
	  ((<= rightA leftB) nil)
	  ((>= leftA rightB) nil)
	  (t t))))

(defmethod draw ((e entity))
  (sdl:draw-box-* (first (pos e)) (second (pos e)) (first (size e)) (second (size e)) :color (color e)))

(defmethod move ((e entity))
 (setf (pos e) (mapcar #'+  (pos e) (vel e))))

(defmethod move ((e coin))
  (setf (pos e) `( ,(random (- 640 16)) ,(random (- 480 16)))))

;;; Global variables
(defparameter *player* (make-instance 'player))
(defparameter *coin* (make-instance 'coin))
(defparameter *score* 0)

;;; Entry point
(sdl:with-init ()
	 (sdl:window 640 480 :title-caption "Coin Collector: LISP Edition")
	 (setf (sdl:frame-rate) 60)
	 (sdl-gfx:initialise-default-font sdl-gfx:*font-9x18*)
	 (sdl:with-events (:poll)
	   (:quit-event () t)
	   (:video-expose-event () (sdl:update-display))
	   (:key-down-event (:key key)
			    (funcall (on-key-down *player*) *player* key))
	   (:key-up-event (:key key)
			  (funcall (on-key-up *player*) *player* key))
	   (:idle ()
		  (sdl:clear-display (sdl:color :r 0 :g 200 :b 0))
		  (move *player*)
		  (if (check-entity-collision *player* *coin*)
		      (funcall (on-collision *player*) *coin*))
		  (draw *player*)
		  (draw *coin*)
		  (sdl-gfx:draw-string-solid-* (concatenate 'string "Score: " (write-to-string *score*)) 0 0)
		  (sdl:update-display))))
