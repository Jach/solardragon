(in-package #:solardragon)

(defclass player (animation-ticker lgame.sprite:sprite)
  ((movement-vec :accessor .movement-vec :initform (vector 0 0))
   (unmoved? :accessor .unmoved? :initform t)
   (state :accessor .state :initform ':waiting :type (member :waiting :spawning :playing :warping :exploding))

   (shield-frame :accessor .shield-frame :initform 0 :documentation "If the shield powerup is active, this will be non-zero, representing the shield frame to draw around the ship (in 1-based indexing)")
   (bullet-time-frame :accessor .bullet-time-frame :initform 0 :documentation "If the bullet-time powerup is active, this will be non-zero, representing the bullet time effect frame to draw around the ship (in 1-based indexing)")

   (smoke-trail-sprites :accessor .smoke-trail-sprites :initform (make-instance 'lgame.sprite:ordered-group))

   (ship-up-img :accessor .ship-up-img)

   (engine-animations :accessor /engine-animations :allocation :class :initform nil)
   (turbo-animations :accessor /turbo-animations :allocation :class)
   (teleport-animations :accessor /teleport-animations :allocation :class)
   (warp-animations :accessor /warp-animations :allocation :class)

   (shield-animations :accessor /shield-animations :allocation :class)
   (bullet-time-animations :accessor /bullet-time-animations :allocation :class)
   ))

(defmethod initialize-instance :after ((self player) &key)
  (when (or (null (/engine-animations self))
            (lgame.texture:.destroyed? (aref (/engine-animations self) 0)))
    (setf (/engine-animations self) (lgame.loader:get-texture-frames-from-horizontal-strip "ship-up-boost1.png" :alpha-blending? t)
          (/turbo-animations self) (lgame.loader:get-texture-frames-from-horizontal-strip "ship-up-boost2.png" :alpha-blending? t)
          (/teleport-animations self) (lgame.loader:get-texture-frames-from-horizontal-strip "ship-teleport.png" :alpha-blending? t)
          (/warp-animations self) (lgame.loader:get-texture-frames-from-horizontal-strip "ship-warp.png" :alpha-blending? t)
          (/shield-animations self) (lgame.loader:get-texture-frames-from-horizontal-strip "bonus-shield.png" :alpha-blending? t)
          (/bullet-time-animations self) (lgame.loader:get-texture-frames-from-horizontal-strip "bonus-bullet.png" :alpha-blending? t)))

  (setf (.ship-up-img self) (get-texture "ship-up.png" :alpha-blending? t)
        (.image self) (.ship-up-img self)
        (.box self) (get-texture-box (.image self)))

  )

(defmethod change-state ((self player) new-state)
  (setf (.state self) new-state)
  (reset-ticks self))

(defmethod update ((self player))
  (tick self)
  (update (.smoke-trail-sprites self))
  (case (.state self)
    (:waiting
      (when-let ((start-pos (check-signal :spawn-player)))
        (setf (box-attr (.box self) :topleft) start-pos)
        (change-state self :spawning)))
    (:spawning
      (let ((frame (truncate (* (.ticks self) +frame-adjust+))))
        (if (>= frame (length (/teleport-animations self)))
            (progn
              (setf (.image self) (.ship-up-img self))
              (when (check-signal :start-player)
                (change-state self :playing)))
            (progn
              (setf (.image self) (aref (/teleport-animations self) frame))
              (when (= frame (1- (length (/teleport-animations self))))
                (send-signal :player-spawned :lifetime ':read-once))))))
    (:playing
      (cond
        ((lgame.event:key-pressed? :key lgame::+sdl-scancode-left+)
         (setf (.unmoved? self) nil)
         (setf (.movement-vec self) (vector -1 0)))
        ((lgame.event:key-pressed? :key lgame::+sdl-scancode-right+)
         (setf (.unmoved? self) nil)
         (setf (.movement-vec self) (vector 1 0)))
        ((lgame.event:key-pressed? :key lgame::+sdl-scancode-up+)
         (setf (.unmoved? self) nil)
         (setf (.movement-vec self) (vector 0 -1)))
        ((lgame.event:key-pressed? :key lgame::+sdl-scancode-down+)
         (setf (.unmoved? self) nil)
         (setf (.movement-vec self) (vector 0 1))))

      (let* ((turbo? (lgame.event:key-pressed? :key lgame::+sdl-scancode-space+))
             (frame (mod (truncate (* (.ticks self) +frame-adjust+)) (length (/engine-animations self))))
             (speed (if turbo? 7 5)) ; turbo speed or normal speed
             (movement-vec (.movement-vec self))
             (box (.box self)))
        (move-box box (* speed (aref movement-vec 0)) (* speed (aref movement-vec 1)))

        (cond
          ((< (box-attr box :top) (box-attr *arena-box* :top))
           (setf (box-attr box :top) (box-attr *arena-box* :top)))
          ((> (box-attr box :bottom) (box-attr *arena-box* :bottom))
           (setf (box-attr box :bottom) (box-attr *arena-box* :bottom)))
          ((< (box-attr box :left) (box-attr *arena-box* :left))
           (setf (box-attr box :left) (box-attr *arena-box* :left)))
          ((> (box-attr box :right) (box-attr *arena-box* :right))
           (setf (box-attr box :right) (box-attr *arena-box* :right))))

        (cond
          ((= (aref movement-vec 0) -1) ; left-moving
           (setf (lgame.sprite:.angle self) -90))
          ((= (aref movement-vec 0) 1)
           (setf (lgame.sprite:.angle self) 90))
          (t
           (setf (lgame.sprite:.angle self) 0)))
        (setf (lgame.sprite:.flip self) (if (= (aref movement-vec 1) 1) ; down
                                            lgame::+sdl-flip-vertical+
                                            lgame::+sdl-flip-none+))

        (setf (.image self) (if (.unmoved? self)
                                (.ship-up-img self)
                                (aref (if turbo? (/turbo-animations self) (/engine-animations self)) frame)))

        (when (check-signal :cubes-collected)
          (change-state self :warping))
        ))
    (:warping
      (setf (lgame.sprite:.flip self) lgame::+sdl-flip-none+
            (lgame.sprite:.angle self) 0
            (.unmoved? self) t
            (.movement-vec self) (vector 0 0))
      (move-box (.box self) 2 -1)
      (let ((frame (truncate (* (.ticks self) +frame-adjust+))))
        (if (= frame (length (/warp-animations self)))
            (progn
              (send-signal :change-level :lifetime ':read-once)
              (change-state self :waiting))
            (setf (.image self) (aref (/warp-animations self) frame)))))
    (:exploding
      ))
  )

(defmethod draw ((self player))
  (unless (eql :waiting (.state self))
    (draw (.smoke-trail-sprites self))
    (when (plusp (.shield-frame self))
      ; draw shield
      )
    (call-next-method)))
