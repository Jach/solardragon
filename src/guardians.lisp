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
        (guard-speed (* 4 (/ 40.0 60.0))))
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
        (move-box (.box self) (- *game-width* 100 32 64) (- *game-height* (* 2 32))))
      (:top
        (setf (aref (.velocity self) 0) guard-speed)
        (move-box (.box self) (* 2 32) 32)
        (setf (lgame.sprite:.flip self) lgame::+sdl-flip-vertical+))
      (:left
        (setf (aref (.velocity self) 1) (- guard-speed))
        (move-box (.box self) 32 (/ *game-height* 2))
        (setf (lgame.sprite:.angle self) 90))
      (:right
        (setf (aref (.velocity self) 1) guard-speed)
        (move-box (.box self) (- *game-width* 100 32 64) (/ *game-height* 2))
        (setf (lgame.sprite:.angle self) -90)))
    (setf (.visual-box self) (lgame.box:copy-box (.box self)))
    (when (or (eql :left pos) (eql :right pos))
      (rotatef (lgame.box:box-width (.visual-box self))
               (lgame.box:box-height(.visual-box self))))

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
             (frame-num (truncate (* (.ticks self) 40/60))))
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
         (when (or (< (box-attr box :left) 32) (> (box-attr box :right) (- *game-width* 100 32))
                   (< (box-attr vbox :top) 32) (> (box-attr vbox :bottom) (- *game-height* 32)))
           (setf (aref (.velocity self) 0) (- (aref (.velocity self) 0)))
           (setf (aref (.velocity self) 1) (- (aref (.velocity self) 1))))
         )))))

;(defmethod draw ((self guardian))
  ;(lgame.render:with-draw-color (0 255 0)
  ;  (lgame.draw:render-fill-box lgame:*renderer* (.visual-box self)))
;  (call-next-method))

; Animation timing from Python? double-check later openframes=8, shootframes=5, totalframes=15
