(in-package #:solardragon)

(defclass guardian (animation-ticker lgame.sprite:sprite lgame.sprite:add-groups-mixin); lgame.sprite:draw-solid-background-mixin)
  ((guard-position :accessor .guard-position :initarg :position :initform :top :type (member :top :bottom :left :right))
   (state :accessor .state :initform :waiting
          :documentation "Has the following states: :waiting, :spawning, :moving, :attacking, :dead. Once spawned, transition to :moving, and occasionally do
                          an attack animation in :attacking state (still moves though) before going back to :moving.
                          :waiting is waiting on the HUD to slide in before spawning. After spawning, they begin to move.
                          A powerup can kill a random guardian, it will be :dead until the next level where it respawns."
          )
   (can-attack? :accessor .can-attack? :initform nil
                :documentation "Additional state that can prevent the guardian from attacking, either because the level hasn't fully started yet or perhaps from a collected powerup.")
   (visual-box :accessor .visual-box :initform nil :documentation "For left and right guardians, the movement and render boxes do not match the end result because of rotation.
                                                                   This box is just a copy that gets updated in sync, but swaps the width/height so it matches the actual sprite bounds.")
   (velocity :accessor .velocity :initform (vector 0 0))

   (animations :accessor /animations :allocation :class :initform (make-hash-table) :documentation "Set of animation name -> sprite frames (spawning, attacking, and exploding)"))
  )

(defmethod initialize-instance :after ((self guardian) &key)
  (let ((anims (/animations self))
        (pos (.guard-position self))
        (guard-speed (* 4 +frame-adjust+)))
    (when (or (zerop (hash-table-count anims))
              (lgame.texture:.destroyed? (aref (gethash :spawning anims) 0))) ; our job to fully (re)initialize it
      (setf (gethash :spawning anims) (lgame.loader:get-texture-frames-from-horizontal-strip "baddie-teleport.png" :frame-width 64 :alpha-blending? t)
            (gethash :attacking anims) (lgame.loader:get-texture-frames-from-horizontal-strip "baddie.png" :frame-width 64 :alpha-blending? t)
            (gethash :exploding anims) (lgame.loader:get-texture-frames-from-horizontal-strip "explosion.png" :alpha-blending? t)))

    (setf (.image self) (aref (gethash :spawning anims) 0)
          (.box self) (get-texture-box (.image self)))

    (case pos
      (:bottom
        (setf (aref (.velocity self) 0) (- guard-speed))
        (setf (box-attr (.box self) :topright) (box-attr *arena-box* :bottomright)))
      (:top
        (setf (aref (.velocity self) 0) guard-speed)
        (setf (box-attr (.box self) :bottomleft) (box-attr *arena-box* :topleft))
        (setf (lgame.sprite:.flip self) lgame::+sdl-flip-vertical+))
      (:left
        (setf (aref (.velocity self) 1) (- guard-speed))
        (setf (lgame.sprite:.angle self) 90))
      (:right
        (setf (aref (.velocity self) 1) guard-speed)
        (setf (lgame.sprite:.angle self) -90)))
    (setf (.visual-box self) (lgame.box:copy-box (.box self)))
    (when (or (eql :left pos) (eql :right pos))
      (rotatef (lgame.box:box-width  (.visual-box self))
               (lgame.box:box-height (.visual-box self))))

    (when (eql pos :left)
        (setf (box-attr (.visual-box self) :bottomright) (box-attr *arena-box* :bottomleft))
        (setf (box-attr (.box self) :center) (box-attr (.visual-box self) :center)))

    (when (eql pos :right)
        (setf (box-attr (.visual-box self) :topleft) (box-attr *arena-box* :topright))
        (setf (box-attr (.box self) :center) (box-attr (.visual-box self) :center)))

   (lgame.sprite:set-alpha self 0.0)
   ))

(defmethod change-state ((self guardian) state)
  (setf (.state self) state)
  (reset-ticks self))

(defmethod move ((self guardian))
  (move-box (.box self) (aref (.velocity self) 0) (aref (.velocity self) 1))
  (move-box (.visual-box self) (aref (.velocity self) 0) (aref (.velocity self) 1)))

(defmethod update ((self guardian))
  (tick self)
  (incf (.elapsed self) (lgame.time:dt))
  (let ((state (.state self)))
    (cond
      ((eql state :waiting)
       (when (check-signal :hud-appeared)
         (lgame.sprite:set-alpha self 1.0)
         (change-state self :spawning)))
      ((eql state :spawning)
       (let ((frames (gethash :spawning (/animations self)))
             (frame-num (truncate (* (.ticks self) +frame-adjust+))))
         (setf (.image self) (aref frames (min frame-num (1- (length frames)))))
         (when (and (plusp frame-num)
                    (zerop (mod frame-num (length frames))))
           (setf (.image self) (aref (gethash :attacking (/animations self)) 0))
           (change-state self :moving))))
      ((or (eql state :moving) (eql state :attacking))
       (when (check-signal :level-started)
         (setf (.can-attack? self) t))
       (let ((box (.box self))
             (vbox (.visual-box self)))
         (move self)
         (if (member (.guard-position self) '(:top :bottom))
             (when (or (< (box-attr box :left) (box-attr *arena-box* :left))
                       (> (box-attr box :right) (box-attr *arena-box* :right)))
               (setf (aref (.velocity self) 0) (- (aref (.velocity self) 0))))

             (when (or (< (box-attr vbox :top) (box-attr *arena-box* :top))
                       (> (box-attr vbox :bottom) (box-attr *arena-box* :bottom)))
               (setf (aref (.velocity self) 1) (- (aref (.velocity self) 1)))))

         )))))

;(defmethod draw ((self guardian))
  ;(lgame.render:with-draw-color (0 255 0)
  ;  (lgame.draw:render-fill-box lgame:*renderer* (.visual-box self)))
;  (call-next-method))

; Animation timing from Python? double-check later openframes=8, shootframes=5, totalframes=15
