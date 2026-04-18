(in-package #:solardragon)

;; Toast messages for level titles and other info.

(defclass message (lgame.sprite:sprite animation-ticker)
  ((text :accessor .text :initarg :text :initform "")
   (visible-duration :accessor .visible-duration :initform 2.0)
   (fade-duration :accessor .fade-duration :initform 0.3)

   (alive-messages :accessor /alive-messages :allocation :class :initform 0)
   ))

(defmethod initialize-instance :after ((self message) &key)
  (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 24))
         (msg-texture (lgame.font:render-text font (.text self) 128 255 255)))
    (lgame.texture:enable-alpha-blending msg-texture)
    (setf (.image self) msg-texture
          (.box self) (get-texture-box (.image self)))
    (setf (box-attr (.box self) :centerx) (box-attr *arena-box* :centerx))
    (setf (box-attr (.box self) :centery) (+ 70 (* (/alive-messages self) (+ (lgame.box:box-height (.box self)) 10))))
    (incf (/alive-messages self))))

(defmethod kill ((self message))
  (decf (/alive-messages self))
  (lgame.texture:destroy-texture (.image self))
  (call-next-method))

(defmethod update ((self message))
  (tick self)
  (let* ((elapsed (.elapsed self))
         (visible-dur (.visible-duration self))
         (fade-in-dur (.fade-duration self))
         (fade-out-mark (- visible-dur fade-in-dur))
         (fade-out-dur (- visible-dur fade-out-mark)))
    (when (<= 0 elapsed fade-in-dur) ; fade in
      (lgame.sprite:set-alpha self (/ elapsed fade-in-dur)))
    (when (>= elapsed fade-out-dur) ; fade out
      (lgame.sprite:set-alpha self (alexandria:clamp (- 1.0 (/ (- elapsed fade-out-mark) fade-out-dur)) 0.0 1.0)))
    (when (>= elapsed visible-dur)
      (kill self))))
