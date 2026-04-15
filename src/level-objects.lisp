(in-package #:solardragon)

;; Manages the cubes / mines / powerups that appear on a level-by-level basis.

(defclass level-objects (lgame.sprite:sprite)
  ((hud :accessor .hud :initarg :hud)
   (cubes :accessor .cubes :initform (make-instance 'lgame.sprite:group))
   (title-text :accessor .title-text :initform nil)

   (state :accessor .state :initform :waiting :type (member :waiting :spawning-cubes :waiting-for-player :playing))

   (cubes-to-spawn :accessor .cubes-to-spawn :initform (list))
   ))

(defun lvl-num (lvl-objects)
  (.current-level (.hud lvl-objects)))

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
          (change-state self :spawning-cubes)
          ))
      (:spawning-cubes
        (alexandria:if-let ((next-cube (pop (.cubes-to-spawn self))))
          (multiple-value-bind (x y) (cube-grid-pos-to-coords (aref next-cube 0) (aref next-cube 1))
            (lgame.sprite:add-sprites (.cubes self)
                                      (make-instance 'collectable-cube :x x :y y :level level-num :cube (get-cube-at level next-cube))))

          (change-state self :waiting-for-player)))
      (:waiting-for-player
        (when (check-signal :hud-bar-filled)
          ; spawn player
          (change-state self :playing)))
      (:playing
        (alexandria:when-let ((next-level (check-signal :change-level)))
          ; verify cubes are dead
          (lgame.sprite:map-sprite #'kill (.cubes self))
          (setf (.current-level (.hud self)) next-level)
          (setup-cube-order self)
          (change-state self :spawning-cubes))
        nil))

    (update (.cubes self))
    ))

(defmethod draw ((self level-objects))
  (lgame.sprite:draw (.cubes self)))

;(send-signal :change-level :datum 31 :lifetime ':read-once)
