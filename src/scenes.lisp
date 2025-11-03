(in-package #:solardragon)

(defvar *current-scene* nil)

(defun scene-change (scene-key)
  (when *current-scene*
    (scene-unload *current-scene*))
  (setf *current-scene*
        (case scene-key
          (:title (make-instance 'title-scene))
          (t *current-scene*))))

(defgeneric scene-receive-event (scene event)
  )

(defgeneric scene-update (scene)
  )

(defgeneric scene-render (scene)
  )

(defgeneric scene-unload (scene)
  )

(defclass scene ()
  ())

;;;; Title scene

(defclass title-scene-item (lgame.sprite:sprite)
  ((pos :accessor .pos :initarg :pos)
   (small-box :accessor .small-box)
   (img-selected :accessor .img-selected)
   (img-unselected :accessor .img-unselected)))

(defmethod initialize-instance :after ((self title-scene-item) &key img-name)
  (setf (.img-selected self) (get-texture (format nil "menu_~a_on.png" img-name))
        (.img-unselected self) (get-texture (format nil "menu_~a_off.png" img-name)))
  (setf (.box self) (get-texture-box (.img-selected self)))
  (setf (box-attr (.box self) :topleft) (.pos self))
  (setf (.small-box self) (get-texture-box (.img-unselected self)))
  (setf (box-attr (.small-box self) :center) (box-attr (.box self) :center))
  (set-item-selected self nil))

(defmethod set-item-selected ((self title-scene-item) selected?)
  (if selected?
      (progn
        (setf (.image self) (.img-selected self))
        (setf (.box self) (get-texture-box (.img-selected self)))
        (setf (box-attr (.box self) :topleft) (.pos self)))
      (progn
        (setf (.image self) (.img-unselected self))
        (setf (.box self) (.small-box self)))))


(defclass title-scene (scene)
  ((selected-item :accessor .selected-item :initform 0)
   (selected-bg-effect :accessor .selected-bg-effect)
   (bg-glow :accessor .bg-glow :initform 0.0)

   (big-box-frame-tick :accessor .big-box-frame-tick :initform 0)

   (item-names :accessor .item-names :initform #("start" "news" "creds" "setup" "quit"))
   (items :accessor .items :initform (make-hash-table :test #'equal))
   (sprites :accessor .sprites :initform (make-instance 'lgame.sprite:ordered-group))))

(defmethod initialize-instance :after ((self title-scene) &key)
  (let* ((logo (make-instance 'lgame.sprite:sprite :image (get-texture "logo.png")))
         (ship (make-instance 'lgame.sprite:sprite :image (get-texture "ship-big.png")))
         (selected-bg-effect (make-instance 'lgame.sprite:sprite :image (get-texture "menu_on_bgd.png")))
         (big-boxes (lgame.loader:get-texture-frames-from-horizontal-strip "bigboxes.png"))
         (big-box-sprite (make-instance 'lgame.sprite:sprite :image (aref big-boxes 0) :box (get-texture-box (aref big-boxes 0) :x 580 :y 80))))
    (setf (.box logo) (get-texture-box (.image logo) :x 30 :y 25)
          (.box ship) (get-texture-box (.image ship) :x 450 :y 250)
          (.box selected-bg-effect) (get-texture-box (.image selected-bg-effect)))
    (lgame.box:inflate-box (.box logo) -2 -2)
    (lgame.sprite:enable-alpha-blending selected-bg-effect)
    (lgame.sprite:set-alpha selected-bg-effect 1.0)
    (setf (.selected-bg-effect self) selected-bg-effect)
    (lgame.sprite:add-sprites (.sprites self)
                              logo
                              ship
                              selected-bg-effect
                              big-box-sprite)
    (setf (gethash "bigbox" (.items self)) big-box-sprite)
    (setf (gethash "bigboxes" (.items self)) big-boxes))

  (apply #'lgame.sprite:add-sprites (.sprites self)
         (loop for item across (.item-names self)
               for x = 20 then (+ x 150)
               for y = 380
               for y-off = nil then (not y-off)
               collect (setf (gethash item (.items self))
                             (make-instance 'title-scene-item
                                            :pos (list x (+ y (if y-off 20 0)))
                                            :img-name item))))

  (select-title-item self "start"))

(defmethod select-title-item ((self title-scene) item)
  (set-item-selected (gethash (aref (.item-names self) (.selected-item self))
                              (.items self))
                     nil) ; unselect current
  (let ((item-sprite (gethash item (.items self))))
    (set-item-selected item-sprite t)
    (lgame.box:set-box (.box (.selected-bg-effect self))
                       :x (lgame.box:box-x (.box item-sprite))
                       :y (lgame.box:box-y (.box item-sprite)))
    (setf (.selected-item self) (position item (.item-names self) :test #'equal))))

(defmethod scene-receive-event ((self title-scene) event)
  (when (= (event-type event) lgame::+sdl-keydown+)
    (when (= (key-scancode event) lgame::+sdl-scancode-left+)
      (select-title-item self (aref (.item-names self) (mod (- (.selected-item self) 1) (length (.item-names self))))))
    (when (= (key-scancode event) lgame::+sdl-scancode-right+)
      (select-title-item self (aref (.item-names self) (mod (+ (.selected-item self) 1) (length (.item-names self))))))
    (when (= (key-scancode event) lgame::+sdl-scancode-return+)
      )
    (when (= (key-scancode event) lgame::+sdl-scancode-escape+)
      (lgame.time:clock-stop))
    ))

(defmethod scene-update ((self title-scene))
  (incf (.bg-glow self) (* 0.35 (/ 40.0 60.0)))
  (lgame.sprite:set-alpha (.selected-bg-effect self)
                          (/ (* 50.0 (+ 2.5 (sin (.bg-glow self))))
                             256))
  ;; animation for spinning box was set at 1 frame of animation per update frame with the game loop updating at.. 40 FPS!
  ;; we're doing at least 60 FPS, so we have to change the frame a bit more slowly than once per frame.
  (let ((big-boxes (gethash "bigboxes" (.items self)))
        (big-box-sprite (gethash "bigbox" (.items self))))
    ;(setf (.big-box-frame-tick self) (mod (+ (.big-box-frame-tick self) 1) (length big-boxes))) ; animation was made for one frame every 16ms aka per tick at 60 FPS
    (setf (.big-box-frame-tick self) (mod (+ (.big-box-frame-tick self) (/ 40.0 60.0)) (length big-boxes)))
    (setf (.image big-box-sprite) (aref big-boxes (truncate (.big-box-frame-tick self))))))

(defmethod scene-render ((self title-scene))
  (lgame.sprite:draw (.sprites self)))

(defmethod scene-unload ((self title-scene))
  )
