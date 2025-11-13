(in-package #:solardragon)

;; The starfield is the game background. We implement it as just a single sprite texture, which every frame we clear and draw pixel-stars to.
;; The pixel-stars move at different rates so we get a cheap but cool effect.

(defstruct stars
  (xs nil :type (simple-array single-float (*)))
  (ys nil :type (simple-array single-float (*)))
  (speeds nil :type (simple-array single-float (*)))
  (colors nil :type (simple-array t (*))))

(defclass starfield (lgame.sprite:sprite)
  ((stars :accessor .stars)
   (max-stars :accessor .max-stars :initform 800)
   (new-star-timer :accessor .new-star-timer :initform 0.0)
   (current-stars :accessor .current-stars :initform 0) ; every time the timer hits 0, more stars are added until max is hit
   (first-half? :accessor .first-half? :initform t) ; to slow down stars further, only half of them are updated and drawn per frame, alternating each frame, which can sometimes give a pseudo-twinkling
   ))

(defmethod initialize-instance :after ((self starfield) &key)
  (setf (.image self) (lgame.texture:create-empty-sdl-texture lgame:*renderer* lgame::+sdl-textureaccess-target+ *game-width* *game-height*))
  (setf (.box self) (lgame.box:get-texture-box (.image self)))

  (let* ((max-stars (.max-stars self))
         (stars (make-stars
                  :xs (make-array max-stars :element-type 'single-float :initial-element 0.0)
                  :ys (make-array max-stars :element-type 'single-float :initial-element 0.0)
                  :speeds (make-array max-stars :element-type 'single-float :initial-element 0.0)
                  :colors (make-array max-stars :initial-element nil))))
    (setf (.stars self) stars)
    (add-stars self 50)))

(defmethod add-stars ((self starfield) amount)
  (loop for i from (.current-stars self) below (min (.max-stars self) (+ amount (.current-stars self)))
        do
        (incf (.current-stars self))
        (let* ((r-val (1+ (random 3)))
               (color (list (+ 60 (* r-val 40))
                            (+ 50 (* r-val 35))
                            (+ 100 (* r-val 22))))
               (speed (* (/ 40.0 60.0) r-val)) ; account for original's 40 fps slower speed per frame
               (x (float (random *game-width*)))
               (y (float (random *game-height*))))
          (setf (aref (stars-xs (.stars self)) i) x
                (aref (stars-ys (.stars self)) i) y
                (aref (stars-speeds (.stars self)) i) speed
                (aref (stars-colors (.stars self)) i) color))))

(defmethod update ((self starfield))
  (decf (.new-star-timer self) (lgame.time:dt))
  (when (<= (.new-star-timer self) 0.0)
    (setf (.new-star-timer self) 0.2) ; more stars in 0.2 secs
    (add-stars self 5))
  (let ((stars (.stars self))
        (lower (if (.first-half? self) 0 (truncate (/ (.current-stars self) 2))))
        (upper (if (.first-half? self) (truncate (/ (.current-stars self) 2)) (.current-stars self))))
    (setf (.first-half? self) (not (.first-half? self)))
    (loop for i from lower below upper do
          ; stars go in a bottom-left direction
          (setf (aref (stars-xs stars) i) (mod (- (aref (stars-xs stars) i)
                                                  (aref (stars-speeds stars) i))
                                               *game-width*))
          (setf (aref (stars-ys stars) i) (mod (+ (aref (stars-ys stars) i)
                                                  (aref (stars-speeds stars) i))
                                               *game-height*)))))

(defmethod draw ((self starfield))
  (let ((stars (.stars self))
        (lower (if (.first-half? self) 0 (truncate (/ (.current-stars self) 2))))
        (upper (if (.first-half? self) (truncate (/ (.current-stars self) 2)) (.current-stars self))))
    (lgame.render:with-render-target (.image self)
      (lgame.render:clear)

      (lgame.rect:with-rect (rect 0 0 1 1)
        (loop for i from lower below upper do
              (lgame.rect:set-rect rect
                                   :x (aref (stars-xs stars) i)
                                   :y (aref (stars-ys stars) i))
              (lgame.render:with-draw-color ((aref (stars-colors stars) i))
                (lgame::sdl-render-fill-rect lgame:*renderer* rect)))))

    (call-next-method)))
