(in-package :cl-snake)

(defvar *game-thread* nil)
(defvar *background-clear-color* (make-color :r 225 :g 225 :b 225 :a 255))
(defvar *grid-line-color* (make-color :r 20 :g 200 :b 200 :a 255))
(defvar *counter* 0)
(defvar *text-color*)

(defvar *frame-counter* 0)
(defvar *game-over* nil)
(defvar *game-paused* nil)
(defvar *counter-trail* 1)
(defvar *allow-move* nil)
(defvar *snake-length* 256)
(defvar *square-size* 31)
(defvar *screen-width* 800)
(defvar *screen-height* 450)
(defvar *screen-title* "classic game: snake")
(defvar *fruit* nil)
(defvar *snake* nil)
(defvar *snake-position* nil)
(defvar *offset* nil)

(defun start ()
  (setf *game-thread* (bt2:make-thread #'game :name "GameThread" :trap-conditions t)))

(defun game ()
  (%init-window *screen-width* *screen-height* *screen-title*)
  (%set-target-fps 60)
  (init-game)
  (let ((*text-color* (make-color :r 200 :g 200 :b 150 :a 255)))
    (loop :until (%window-should-close)
	  :do
	     (update)
	     (draw-game))
    )
  (%close-window))

(defun stop ()
  (bt2:destroy-thread *game-thread*))

(defstruct snake
  (position (make-vec2) :type vec2)
  (size (make-vec2 :x (float *square-size*) :y (float *square-size*)) :type vec2)
  (speed (make-vec2 :x (float *square-size*) :y (float 0)) :type vec2)
  (color (make-color :r 0 :g 0 :b 0) :type color))

(defstruct food
  (position (make-vec2) :type vec2)
  (size (make-vec2) :type vec2)
  (active nil :type boolean)
  (color (make-color :r 0 :g 0 :b 0) :type color))

(defun init-game ()
  (setf *frame-counter* 0
	*game-over* nil
	*game-paused* nil
	*counter-trail* 1
	*allow-move* nil
	*offset* (make-vec2 :x (float (mod *screen-width* *square-size*))
			    :y (float (mod *screen-height* *square-size*)))
	*snake* (make-array *snake-length* :element-type 'snake :adjustable t :fill-pointer 0)
	*snake-position* (make-array *snake-length* :element-type 'vec2 :adjustable t :fill-pointer 0)
	*fruit* (make-food :size (make-vec2 :x (float *square-size*) :y (float *square-size*))
			   :color (make-color :r 25 :g 150 :b 0)
			   :active nil))
  
  (loop :for i :from 0 :below *snake-length*
	:do (let ((a-snake (make-snake)))
	      (setf (vec2-x (snake-position a-snake)) (/ (vec2-x *offset*) 2)
		    (vec2-y (snake-position a-snake)) (/ (vec2-y *offset*) 2)
		    (snake-color a-snake) (make-color :r 100 :g 100 :b 150))
	      (vector-push a-snake *snake*)))
  
  (setf (snake-color (aref *snake* 0)) (make-color :r 0 :g 50 :b 125 )))

(defun update ()
  (unless *game-over*
    (and (%is-key-pressed :key-p)
	 (setf *game-paused* (not *game-paused*)))
    
    (unless *game-paused*

      (cond
	((and (%is-key-pressed :key-right)
	      (eq (float 0) (vec2-x (snake-speed (aref *snake* 0))))
	      *allow-move*)
	 (setf (vec2-x (snake-speed (aref *snake* 0))) (float *square-size*)
	       (vec2-y (snake-speed (aref *snake* 0))) (float 0)
	       *allow-move* nil))

	((and (%is-key-pressed :key-left)
	      (eq (float 0) (vec2-x (snake-speed (aref *snake* 0))))
	      *allow-move*)
	 (setf (vec2-x (snake-speed (aref *snake* 0))) (float (- *square-size*))
	       (vec2-y (snake-speed (aref *snake* 0))) (float 0)
	       *allow-move* nil))

	((and (%is-key-pressed :key-up)
	      (eq (float 0) (vec2-y (snake-speed (aref *snake* 0))))
	      *allow-move*)
	 (setf (vec2-y (snake-speed (aref *snake* 0))) (float (- *square-size*))
	       (vec2-x (snake-speed (aref *snake* 0))) (float 0)
	       *allow-move* nil))

	((and (%is-key-pressed :key-down)
	      (eq (float 0) (vec2-y (snake-speed (aref *snake* 0))))
	      *allow-move*)
	 (setf (vec2-y (snake-speed (aref *snake* 0))) (float *square-size*)
	       (vec2-x (snake-speed (aref *snake* 0))) (float 0)
	       *allow-move* nil)))

      (loop :for n :from 0 :below *counter-trail*
	    :do (setf (aref *snake-position* n) (copy-vec2 (snake-position (aref *snake* n)))))

      (if (eq 0 (mod *frame-counter* 25))
	  (let ((pos-x (vec2-x (snake-position (aref *snake* 0))))
		(pos-y (vec2-y (snake-position (aref *snake* 0)))))
	    (setf pos-x (+ pos-x (vec2-x (snake-speed (aref *snake* 0))))
		  pos-y (+ pos-y (vec2-y (snake-speed (aref *snake* 0))))
		  (vec2-x (snake-position (aref *snake* 0))) pos-x
		  (vec2-y (snake-position (aref *snake* 0))) pos-y)

	    (setf *allow-move* t)

	    (loop :for n :from 1 :below *counter-trail*
		  :do (setf (snake-position (aref *snake* n)) (copy-vec2 (aref *snake-position* (1- n)))))
	    
	    (loop :for n :from 1 :below *counter-trail*
		  :do (if (equalp (snake-position (aref *snake* 0)) (snake-position (aref *snake* n)))
			  (progn
			    (setf *game-over* t))))	    
	    
	    )
	  
	        
	  )

      (let ((out-of-bounds (or (> (float (vec2-x (snake-position (aref *snake* 0))))
				  (float (- *screen-width* (vec2-x *offset*))))
			       (> (float (vec2-y (snake-position (aref *snake* 0))))
				  (float (- *screen-height* (vec2-y *offset*))))
			       (< (float (vec2-x (snake-position (aref *snake* 0)))) (float 0))
			       (< (float (vec2-y (snake-position (aref *snake* 0)))) (float 0)))))
	(if out-of-bounds (setf *game-over* t)))

      (unless (food-active *fruit*)
	(setf (food-active *fruit*) t)
	(setf (vec2-x (food-position *fruit*)) (float (+ (* (random (round (1- (/ *screen-width* *square-size*))))
							    *square-size*)
							 (/ (vec2-x *offset*) 2)))
	      (vec2-y (food-position *fruit*)) (float (+ (* (random (round (1- (/ *screen-height* *square-size*))))
							    *square-size*)
							 (/ (vec2-y *offset*) 2))))

	(loop :for n :from 0 :below *counter-trail*
	      :do
		 (loop :while (equalp (snake-position (aref *snake* n))
				      (food-position *fruit*))
		       :do
			  (setf (vec2-x (food-position *fruit*)) (float (+ (* (random (round (1- (/ *screen-width*
												    *square-size*))))
									      *square-size*)
									   (/ (vec2-x *offset*) 2)))
				(vec2-y (food-position *fruit*)) (float (+ (* (random (round (1- (/ *screen-height*
												    *square-size*))))
									      *square-size*)
									   (/ (vec2-y *offset*) 2)))
				n 0))))
	
      (let ((fruit-consumed (and (< (vec2-x (snake-position (aref *snake* 0)))
				    (+ (vec2-x (food-position *fruit*))
				       (vec2-x (food-size *fruit*))))
				 (> (+ (vec2-x (snake-position (aref *snake* 0)))
				       (vec2-x (snake-size (aref *snake* 0))))
				    (vec2-x (food-position *fruit*)) )
				 (< (vec2-y (snake-position (aref *snake* 0)))
				    (+ (vec2-y (food-position *fruit*))
				       (vec2-y (food-size *fruit*))))
				 (> (+ (vec2-y (snake-position (aref *snake* 0)))
				       (vec2-y (snake-size (aref *snake* 0))))
				    (vec2-y (food-position *fruit*)) ))))
	(if fruit-consumed
	    (setf (snake-position (aref *snake* *counter-trail*))
		  (snake-position (aref *snake* (1- *counter-trail*)))

		  *counter-trail* (1+ *counter-trail*)
		  (food-active *fruit*) nil))
	)

      (setf *frame-counter* (1+ *frame-counter*))))

  (if (and *game-over* (%is-key-pressed :key-enter))
      (progn
	(init-game)
	(setf *game-paused* nil)
	)))


(defun draw-game ()
  (%begin-drawing)
  (%clear-background *background-clear-color*)
  (unless *game-over*
    (loop :for n :from 0 :below (1+ (/ *screen-width* *square-size*))
	  :do (%draw-line-v (make-vec2 :x (float (+ (* *square-size* n) (/ (vec2-x *offset*) 2)))
				       :y (float (/ (vec2-y *offset*) 2)))
			    (make-vec2 :x (float (+ (* *square-size* n) (/ (vec2-x *offset*) 2)))
				       :y (float (- *screen-height* (/ (vec2-y *offset*) 2))))
			    *grid-line-color*))

    (loop :for n :from 0 :below (1+ (/ *screen-height* *square-size*))
	  :do (%draw-line-v (make-vec2 :x (float (/ (vec2-x *offset*) 2))
				       :y (float (+ (* *square-size* n) (/ (vec2-y *offset*) 2))))
			    (make-vec2 :x (float (- *screen-width* (/ (vec2-x *offset*) 2)))
				       :y (float (+ (* *square-size* n) (/ (vec2-y *offset*) 2))))
			    *grid-line-color*))

    (loop :for n :from 0 :below *counter-trail*
	  :do (%draw-rectangle-v (snake-position (aref *snake* n))
				 (snake-size (aref *snake* n))
				 (snake-color (aref *snake* n))))

    (%draw-rectangle-v (food-position *fruit*)
		       (food-size *fruit*)
		       (food-color *fruit*))

    (if *game-paused*
	(%draw-text "Game paused"
		    (- (/ *screen-width* 2) (/ (%measure-text "Game paused" 40) 2))
		    (- (/ *screen-height* 2) 40 )
		    40 *text-color*))
    )
  (if *game-over*
      (%draw-text "Press [ENTER] to play again"
		  (- (/ (%get-screen-width) 2) (%measure-text "Press [ENTER] to play again" 20))
		  (- (/ (%get-screen-height) 2) 50)
		  20 *text-color*))
  (%end-drawing))
