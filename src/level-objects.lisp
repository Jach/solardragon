(in-package #:solardragon)

;; Manages the cubes / mines / powerups that appear on a level-by-level basis.

(defclass level-objects (lgame.sprite:sprite)
  ((hud :accessor .hud :initarg :hud)
   (player :accessor .player :initarg :player)
   (cubes :accessor .cubes :initform (make-instance 'lgame.sprite:group))
   (title-text :accessor .title-text :initform nil)

   (state :accessor .state :initform :waiting :type (member :waiting :spawning-cubes :waiting-for-player :playing :level-transition))

   (cubes-to-spawn :accessor .cubes-to-spawn :initform (list))
   ))

(defun lvl-num (self)
  (.current-level (.hud self)))

(defmethod initialize-instance :after ((self level-objects) &key)
  (setup-cube-order self))

(defun setup-cube-order (self)
  (let* ((level (aref *level-data* (mod (lvl-num self) (length *level-data*))))
         (cubes-to-spawn (alexandria:shuffle (alexandria:hash-table-keys (.cube-locations level)))))
    (setf (.cubes-to-spawn self) cubes-to-spawn)))

(defmethod change-state ((self level-objects) state)
  (setf (.state self) state))

(defun cube-grid-pos-to-coords (row col)
  "Returns pixel x,y as multiple values for the top left coordinate of the row and col given by the 7x9 cube grid"
  (let ((cell-size 58)
        (offset-x 106)
        (offset-y 106))
    (values (+ offset-x (* col cell-size))
            (+ offset-y (* row cell-size)))))

(defmethod update ((self level-objects))
  (let* ((level-num (lvl-num self))
         (level (aref *level-data* (mod level-num (length *level-data*)))))
    (case (.state self)
      (:waiting
        (when (check-signal :hud-lives-loaded) ; show title text and begin spawning in cubes one at a time in ramdom order
          (unless (.title-text self)
            ; should also spawn 'Begin' for special case of first level...
            (setf (.title-text self) 1)); (make-instance 'message :text (.title or secondary-title if mirrored.. level)
          (send-signal :spawning-new-level-cubes :lifetime ':read-once)
          (change-state self :spawning-cubes)
          ))
      (:spawning-cubes
        (if-let ((next-cube (pop (.cubes-to-spawn self))))
          (multiple-value-bind (x y) (cube-grid-pos-to-coords (aref next-cube 0) (aref next-cube 1))
            (lgame.sprite:add-sprites (.cubes self)
                                      (make-instance 'collectable-cube :x x :y y :level level-num :cube-type (get-cube-at level next-cube) :player (.player self))))

          (let ((player-pos (multiple-value-list (cube-grid-pos-to-coords (.start-row level) (.start-col level)))))
            (send-signal :spawn-player :datum player-pos :lifetime ':read-once)
            (change-state self :waiting-for-hud))))
      (:waiting-for-hud
        (when (check-signal :hud-bar-filled)
          (change-state self :waiting-for-player)))
      (:waiting-for-player
        (when (check-signal :player-spawned)
          (send-signal :start-timer :lifetime ':read-once) ; once for timer
          (send-signal :start-player :lifetime ':read-once) ; once for player...
          (change-state self :playing)))
      (:playing
        (when (zerop (lgame.sprite:sprite-count (.cubes self)))
          (send-signal :cubes-collected :lifetime ':read-once)
          (change-state self :level-transition)))
      (:level-transition
        (when-let ((next-level (check-signal :change-level)))
          (if (numberp next-level)
              (setf (.current-level (.hud self)) next-level)
              (incf (.current-level (.hud self))))
          (setup-cube-order self)
          (send-signal :spawning-new-level-cubes :lifetime ':read-once)
          (change-state self :spawning-cubes))
        nil))

    (update (.cubes self))
    ))

(defmethod draw ((self level-objects))
  (lgame.sprite:draw (.cubes self)))
