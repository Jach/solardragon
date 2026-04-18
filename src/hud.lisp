(in-package #:solardragon)

(defclass hud (lgame.sprite:sprite animation-ticker)
  ((time-empty-img :accessor .time-empty-img)
   (time-full-img :accessor .time-full-img)
   (mini-ship-img :accessor .mini-ship-img)
   (mini-ship-box :accessor .mini-ship-box)
   (level-number-imgs :accessor .level-number-imgs :initform (make-hash-table :test #'equal))
   (level-number-boxes :accessor .level-number-boxes :initform (make-hash-table :test #'equal))

   (state :accessor .state :documentation
          "Has 5 states. They are :appearing, :appearing-lives, :filling, :waiting, and :draining.
           The first is when the game starts, we slide the empty hud in from the right, then change to :appearing-lives and draw the lives one by one,
           then transition to the :filling state. The :filling state fills the time bar up to max, then transitions to the :waiting state. ?
           In the :draining state, the skip-a-level bar drains as time passes.")

   (time-percent :accessor .time-percent :initform 0.0) ; how full the time meter is
   (fill-duration :accessor .fill-duration :initform 1.2)
   (drain-duration :accessor .drain-duration :initform 10.0)
   (drain-speed-mod :accessor .drain-speed-mod :initform 1.0)

   (lives :accessor .lives :initform 0)
   (current-level :accessor .current-level :initform 0)
   (current-level-imgs :accessor .current-level-imgs :initform nil)
   (current-level-boxes :accessor .current-level-boxes :initform nil)
   ))

(defmethod initialize-instance :after ((self hud) &key)
  (setf (.time-empty-img self) (get-texture "hud-empty.png" :alpha-blending? t)
        (.time-full-img self) (get-texture "hud-full.png")
        (.mini-ship-img self) (get-texture "ship-mini-boost2.png")
        (gethash "I" (.level-number-imgs self)) (get-texture "score_1.png")
        (gethash "V" (.level-number-imgs self)) (get-texture "score_5.png")
        (gethash "X" (.level-number-imgs self)) (get-texture "score_10.png")
        (gethash "L" (.level-number-imgs self)) (get-texture "score_50.png")
        (.state self) :appearing
        (.box self) (get-texture-box (.time-empty-img self))
        (.mini-ship-box self) (get-texture-box (.mini-ship-img self)))
  (maphash (lambda (k v) (setf (gethash k (.level-number-boxes self)) (get-texture-box v)))
           (.level-number-imgs self))
  (move-box (.box self) *game-width* 0))

(defmethod change-state :before (obj new-state)
  (fc:record :state-change (list (class-of obj) :from (.state obj) :to new-state)))

(defmethod change-state ((self hud) new-state)
  (reset-ticks self)
  (setf (.state self) new-state))

(defmethod (setf .current-level) :after (new-value (self hud))
  "Redo the list of current-level-imgs to be drawn for the new level number."
  (let ((roman (format nil "~@R" new-value))
        (total-width 0)
        (textures (list))
        (boxes (list)))
    (loop for char across roman
          for key = (string char)
          for glyph = (gethash key (.level-number-imgs self))
          for box = (lgame.box:copy-box (gethash key (.level-number-boxes self)))
          do
          (incf total-width (box-width box))
          (push glyph textures)
          (push box boxes))
    (setf (.current-level-imgs self) (nreverse textures)
          (.current-level-boxes self) (nreverse boxes))
    ;; now adjust box offsets so the center is always at the same spot
    (let* ((hud-offset (- *game-width* (box-width (.box self))))
           (full-box (lgame.box:make-box hud-offset 0 total-width 0)))
      (setf (box-attr full-box :centerx) (+ hud-offset 50))
      (let ((x-offset (box-x full-box)))
        (loop for box in (.current-level-boxes self)
              do
              (setf (box-attr box :midleft) (list x-offset 565))
              (incf x-offset (box-width box)))))))

(defmethod update ((self hud))
  (tick self)
  (case (.state self)
    (:appearing
      ; hud img is 100 px wide, so we want to move it left 100 px within some duration
      (let* ((duration 0.8)
             (start-x *game-width*)
             (target-x (- start-x (box-width (.box self))))
             (progress (min (/ (.elapsed self) duration) 1.0)))
        (setf (box-x (.box self)) (- start-x (* progress (- start-x target-x))))
        (when (>= (.elapsed self) duration)
          (send-signal :hud-appeared)
          (change-state self :appearing-lives))))
    (:appearing-lives
      ; initial lives are 3, make them appear one by one in n ticks
      (when (and (zerop (mod (.ticks self) 20))
                 (< (.lives self) *start-lives*))
        (incf (.lives self)))
      (when (= (.lives self) *start-lives*)
        (send-signal :hud-lives-loaded)
        (change-state self :waiting)))
    (:filling
      (setf (.time-percent self) (/ (.elapsed self) (.fill-duration self))) ; n secs to fill
      (when (>= (.time-percent self) 1.0)
        (setf (.drain-speed-mod self) 1.0)
        (send-signal :hud-bar-filled :lifetime ':read-once)
        (change-state self :waiting)))
    (:draining
      (setf (.time-percent self) (- 1 (/ (.elapsed self) (* (.drain-speed-mod self) (.drain-duration self))))) ; n secs to drain
      (when (<= (.time-percent self) 0.0)
        (change-state self :waiting)))
    (:waiting
      nil))
  )

(defmethod draw ((self hud))
  (let ((box (.box self))
        (mini-ship-box (.mini-ship-box self)))

    ;; draw the time bar
    (lgame.render:blit (.time-full-img self) box)
    (let* ((full-percent (.time-percent self))
           (bar-bottom-pos 480)  ; empty position
           (bar-top-pos 107) ; full position
           (empty-box (lgame.box:copy-box box)))
      (setf (box-attr empty-box :bottom) (- bar-bottom-pos (* full-percent (- bar-bottom-pos bar-top-pos))))

      (lgame.render:with-draw-color (#x20 #x20 #x20)
        (lgame.draw:render-fill-box lgame:*renderer* empty-box)))
    (lgame.render:blit (.time-empty-img self) box)

    ;; draw the wolf eyes
    (unless (or (eql :appearing (.state self))
                (<= (.time-percent self) 0))
      (let ((wolf-box (lgame.box:make-box  16 57 37 19)))
        (lgame.box:with-moved-box (dest wolf-box (box-x box) 0)
          (lgame.render:blit (.time-full-img self) dest wolf-box))))

    ;; draw the lives
    ; for lives, draw up to 3 ships but for >3 draw one ship + number text
    (if (<= (.lives self) 3)
        (dotimes (i (.lives self))
          (lgame.box:with-moved-box (lives-box mini-ship-box
                                               (+ 10 (* i (box-width mini-ship-box)) (box-x box))
                                               510)
            (lgame.render:blit (.mini-ship-img self) lives-box)))

        (let* ((font (lgame.font:load-font (lgame.font:get-default-font) 30))
               (lives-text (lgame.font:render-text font (format nil "x ~a" (.lives self)) 150 200 150)))
          (lgame.box:with-moved-box (lives-box mini-ship-box
                                               (+ 10 4 (box-x box))
                                               510)
            (lgame.render:blit (.mini-ship-img self) lives-box)
            (move-box lives-box (+ 10 (box-width mini-ship-box)) 0)
            (lgame.render:blit lives-text lives-box)
            (lgame.texture:destroy-texture lives-text))))

    ;; draw the level number / score
    (loop for lvl-img in (.current-level-imgs self)
          for lvl-box in (.current-level-boxes self)
          do
          (lgame.render:blit lvl-img lvl-box))))
