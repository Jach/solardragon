(in-package #:solardragon)

;; Manages the cubes / mines / powerups that appear on a level-by-level basis.

(defclass level-objects (lgame.sprite:sprite animation-ticker)
  ((hud :accessor .hud :initarg :hud)
   (player :accessor .player :initarg :player)
   (cubes :accessor .cubes :initform (make-instance 'lgame.sprite:group))
   (popped-cubes :accessor .popped-cubes :initform (make-instance 'lgame.sprite:group))
   (messages :accessor .messages :initform (make-instance 'lgame.sprite:ordered-group))

   (skip-after-spawning? :accessor .skip-after-spawning? :initform nil)

   (state :accessor .state :initform :waiting :type (member :waiting :spawning-cubes :waiting-for-player :playing :level-transition))

   (cube-spawn-time :accessor .cube-spawn-time :initform 0.1)

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
  (reset-ticks self)
  (setf (.state self) state))

(defun toast (self msg)
  (let ((message (make-instance 'message :text msg)))
    (lgame.sprite:add-sprites (.messages self) message)))

(defun cube-grid-pos-to-coords (row col)
  "Returns pixel x,y as multiple values for the top left coordinate of the row and col given by the 7x9 cube grid"
  (let ((cell-size 58)
        (offset-x 106)
        (offset-y 106))
    (values (+ offset-x (* col cell-size))
            (+ offset-y (* row cell-size)))))

(defmethod update ((self level-objects))
  (tick self)
  (let* ((level-num (lvl-num self))
         (hud (.hud self))
         (mirrored?-data-level-num (multiple-value-list (truncate level-num (length *level-data*))))
         (mirrored? (eql 1 (first mirrored?-data-level-num)))
         (data-level-num (second mirrored?-data-level-num))
         (level (aref *level-data* data-level-num)))
    (case (.state self)
      (:waiting
        (when (check-signal :hud-lives-loaded) ; show title text and begin spawning in cubes one at a time in ramdom order
          (change-state hud :filling)
          (change-state self :spawning-cubes)
          (toast self "Begin")
          ))
      (:spawning-cubes
        (when (= (.ticks self) 1)
          (if mirrored?
              (toast self (.secondary-title level))
              (toast self (.title level)))
          )
        (when (>= (.elapsed self) (.cube-spawn-time self))
          (decf (.elapsed self) (.cube-spawn-time self))
          (if-let ((next-cube (pop (.cubes-to-spawn self))))
            (multiple-value-bind (x y) (cube-grid-pos-to-coords (aref next-cube 0) (aref next-cube 1))
              (lgame.sprite:add-sprites (.cubes self)
                                        (make-instance 'collectable-cube :x x :y y :level level-num :cube-type (get-cube-at level next-cube) :player (.player self))))

            (if (.skip-after-spawning? self)
                (progn
                  (when (lgame.sprite:empty? (.messages self)) ; ensure stage title is gone...
                    (setf (.skip-after-spawning? self) nil)
                    (setup-cube-order self)
                    (let ((progress (- 1.0 (.time-percent hud))))
                      (change-state hud :draining)
                      (setf (.drain-speed-mod hud) 0.2)
                      (setf (.elapsed hud) (* progress 0.2 (.drain-duration hud))))
                    (toast self "Level Skipped")
                    (change-state self :despawning-cubes)))

                (let ((player-pos (multiple-value-list (cube-grid-pos-to-coords (.start-row level) (.start-col level)))))
                  (send-signal :spawn-player :datum player-pos :lifetime ':read-once)
                  (change-state self :waiting-for-hud))))))
      (:despawning-cubes
        (when (>= (.elapsed self) (.cube-spawn-time self))
          (decf (.elapsed self) (.cube-spawn-time self))
          (if (not (lgame.sprite:empty? (.cubes self)))
              (let ((cube (lgame.sprite:pop-sprite (.cubes self))))
                (lgame.sprite:add-sprites (.popped-cubes self) cube)
                (cube-hit cube)
                (setf (.hp cube) 0))
              (progn
                (when (lgame.sprite:empty? (.messages self)) ; ensure level skipped text is gone
                  (send-signal :change-level :datum :skipped-last :lifetime ':read-once)
                  (change-state self :level-transition)))))
        )
      (:waiting-for-hud
        (when (check-signal :hud-bar-filled)
          (change-state self :waiting-for-player)))
      (:waiting-for-player
        (when (check-signal :player-spawned)
          (change-state hud :draining)
          (send-signal :start-player :lifetime ':read-once) ; once for player...
          (change-state self :playing)))
      (:playing
        (when (zerop (lgame.sprite:sprite-count (.cubes self)))
          (send-signal :cubes-collected :lifetime ':read-once)
          (change-state self :level-transition)))
      (:level-transition
        (when-let ((next-level (check-signal :change-level)))
          (if (numberp next-level)
              (setf (.current-level hud) next-level)
              (incf (.current-level hud)))
          (setup-cube-order self)
          (let ((time-left (.time-percent hud)))
            (if (or (<= time-left 0.0) (eql :skipped-last next-level))
                (change-state hud :filling)
                (progn
                  (change-state hud :waiting)
                  (setf (.skip-after-spawning? self) t)))
            (change-state self :spawning-cubes)))))

    (update (.cubes self))
    (update (.popped-cubes self))
    (update (.messages self))
    ))

(defmethod draw ((self level-objects))
  (lgame.sprite:draw (.cubes self))
  (lgame.sprite:draw (.popped-cubes self))
  (lgame.sprite:draw (.messages self)))
