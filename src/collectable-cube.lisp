(in-package #:solardragon)

;; Mini cubes collected by the player

(defclass collectable-cube (animation-ticker lgame.sprite:sprite)
  ((hp :accessor .hp :initform 0)
   (player-moved-off :accessor .player-moved-off :initform t :documentation "Set to nil when player hits/collects cube, player must move off for collision to register again")
   (collecting-frames :accessor .collecting-frames :initform 0)
   (offset-frames :accessor .offset-frames :initform 0)
   (rotation-speed :accessor .rotation-speed :initform (+ 1.5 (random 2.5)))
   (rotation :accessor .rotation :initform (random 90.0))

   (player :accessor .player :initarg :player)

   (cube-animation-green :accessor /cube-animation-green :allocation :class :initform nil)
   (cube-animation-red :accessor /cube-animation-red :allocation :class :initform nil)
   (cube-animation-yellow :accessor /cube-animation-yellow :allocation :class :initform nil)
   (cube-animation-white :accessor /cube-animation-white :allocation :class :initform nil)
   ))

(defmethod initialize-instance :after ((self collectable-cube) &key x y level cube-type)
  (when (or (null (/cube-animation-green self))
            (lgame.texture:.destroyed? (aref (/cube-animation-green self) 0)))
    (setf (/cube-animation-green self) (lgame.loader:get-texture-frames-from-horizontal-strip "boxes.png")
          (/cube-animation-red self) (lgame.loader:get-texture-frames-from-horizontal-strip "boxes-red.png")
          (/cube-animation-yellow self) (lgame.loader:get-texture-frames-from-horizontal-strip "boxes-yellow.png")
          (/cube-animation-white self) (lgame.loader:get-texture-frames-from-horizontal-strip "boxes-white.png")))
  (setf (.offset-frames self) (random (length (/cube-animation-green self))))
  (when (zerop (random 2))
    (setf (.rotation-speed self) (- (.rotation-speed self))))
  (let ((hp 1))
    (when (eql :super-cube cube-type)
      (incf hp))
    (when (>= level (/ (length *level-data*) 2)) ; second half
      (incf hp))
    (setf (.hp self) hp))

  (setf (.image self) (aref (/cube-animation-green self) 0)
        (.box self) (get-texture-box (.image self)))
  (move-box (.box self) x y))

(defun pick-animation-set (self)
  (let ((hp (.hp self)))
    (cond
      ((or (< (.ticks self) 2) (plusp (.collecting-frames self)) (zerop hp)) ; spawn / die / collecting color
       (/cube-animation-white self))

      ((= hp 3)
       (/cube-animation-red self))

      ((= hp 2)
       (/cube-animation-yellow self))

      (t
       (/cube-animation-green self)))))


(defmethod update ((self collectable-cube))
  (tick self)
  (when (plusp (.collecting-frames self))
    (decf (.collecting-frames self)))
  ;; check collision with player (and player must move off
  (let ((player-collide? (lgame.box:boxes-collide? (.box self) (.box (.player self)))))
    (when (and player-collide? (.player-moved-off self))
      (setf (.player-moved-off self) nil)
      (decf (.hp self))
      (setf (.collecting-frames self) 3))
    (when (and (not player-collide?) (not (.player-moved-off self)))
      (setf (.player-moved-off self) t)))
  ;; check death
  (when (and (zerop (.hp self)) (zerop (.collecting-frames self)))
    (kill self))

  (incf (.rotation self) (* (.rotation-speed self)  0.15))
  (let* ((frames (pick-animation-set self))
         (frame-num (mod (+ (truncate (.rotation self)) (.offset-frames self)) (length frames))))
    (setf (.image self) (aref frames frame-num))))

