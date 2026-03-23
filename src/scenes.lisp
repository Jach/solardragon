(in-package #:solardragon)

(defvar *current-scene* nil)
(defvar *scene-ready-to-change* nil
  "NIL if no scene change has been requested, otherwise a keyword identifying the scene to change to.")
(defvar *scene-state-to-transfer* nil
  "A plist of scene data that the scene being unloaded thinks the new scene being changed to may want to use.
   This could also be used for debugging purposes.
   Its lifetime is valid during a scene change but is reset to NIL after the new current scene has been bound (and constructor run).")

(defvar *starfield* nil
  "Special game bg that is mainly always present in every scene, just bind it globally after creating it.")

(defun scene-change (scene-key)
  "Request a scene change identified by key. The key roughly maps to a class of the name key-scene.
   Change happens immediately if there's no current scene."
  (if *current-scene*
    (setf *scene-ready-to-change* scene-key)

    (set-scene scene-key)))

(defun set-scene (key)
  (setf *current-scene* (case key
                          (:title (make-instance 'title-scene))
                          (:play (make-instance 'play-scene))
                          (t *current-scene*))
        *scene-ready-to-change* nil
        *scene-state-to-transfer* nil))


(defgeneric scene-receive-event (scene event)
  (:documentation
    "Called automatically by the main loop's event processing loop, once for each event."))

(defgeneric scene-update (scene)
  (:documentation
    "Called automatically by the main loop once after event processing."))

(defgeneric scene-render (scene)
  (:documentation
    "Called automatically by the main loop once after updating."))

(defgeneric scene-unload (scene)
  (:documentation
    "Called automatically by the main loop when a scene-change has been requested.
     The implementing method should set the unloaded? slot to t when the scene is fully unloaded to allow the scene change to finalize.
     But it doesn't have to do so on the first call. If it doesn't, it will be called again in the next frame, and so on, until the unloaded? slot is set.
     This allows for scene transition effects."))

(defclass scene ()
  ((unloaded? :accessor .unloaded? :initform nil :documentation "Should be set by scene-unload once all unloading/transition effects have taken place."))
  )

(defmethod scene-receive-event ((self scene) event)
  "Default implementation does nothing; main loop handles the QUIT event by itself.")

(defmethod scene-unload ((self scene))
  "Default implementation immediately signals the scene is unloaded."
  (setf (.unloaded? self) t))

;;;; Title scene

(defclass title-scene-item (lgame.sprite:sprite)
  ((pos :accessor .pos :initarg :pos)
   (small-box :accessor .small-box)
   (img-selected :accessor .img-selected)
   (img-unselected :accessor .img-unselected)))

(defmethod initialize-instance :after ((self title-scene-item) &key img-name)
  (setf (.img-selected self) (lgame.texture:set-alpha (get-texture (format nil "menu_~a_on.png" img-name) :alpha-blending? t) 1)
        (.img-unselected self) (lgame.texture:set-alpha (get-texture (format nil "menu_~a_off.png" img-name) :alpha-blending? t) 1))
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

   (unload-frames :accessor .unload-frames :initform 13)

   (item-names :accessor .item-names :initform #("start" "news" "creds" "setup" "quit"))
   (items :accessor .items :initform (make-hash-table :test #'equal))

   (sprites :accessor .sprites :initform (make-instance 'lgame.sprite:ordered-group))))

(defun init-unload-frames ()
  "Just some MOP wankery to avoid putting the unload-frames initform in its own variable or just typing the value again in scene-unload."
  (some (lambda (slot) (and (equal 'unload-frames (closer-mop:slot-definition-name slot))
                            (closer-mop:slot-definition-initform slot)))
        (closer-mop:class-direct-slots (find-class 'title-scene))))

(defmethod initialize-instance :after ((self title-scene) &key)
  (let* ((logo (make-instance 'lgame.sprite:sprite :image (get-texture "logo.png" :alpha-blending? t)))
         (ship (make-instance 'lgame.sprite:sprite :image (get-texture "ship-big.png" :alpha-blending? t)))
         (selected-bg-effect (make-instance 'lgame.sprite:sprite :image (get-texture "menu_on_bgd.png" :alpha-blending? t)))
         (big-boxes (lgame.loader:get-texture-frames-from-horizontal-strip "bigboxes.png" :alpha-blending? t))
         (big-box-sprite (make-instance 'lgame.sprite:sprite :image (aref big-boxes 0) :box (get-texture-box (aref big-boxes 0) :x 580 :y 80))))
    (setf (.box logo) (get-texture-box (.image logo) :x 30 :y 25)
          (.box ship) (get-texture-box (.image ship) :x 450 :y 250)
          (.box selected-bg-effect) (get-texture-box (.image selected-bg-effect)))
    (lgame.box:inflate-box (.box logo) -2 -2)
    ;(lgame.sprite:enable-alpha-blending selected-bg-effect)
    (lgame.sprite:set-alpha selected-bg-effect 1.0)
    (setf (.selected-bg-effect self) selected-bg-effect)
    (unless *starfield*
      (setf *starfield* (make-instance 'starfield)))
    (lgame.sprite:add-sprites (.sprites self)
                              ship ; drawn behind starfield in original which gives an interesting effect of seeing stars on the windows
                              *starfield*
                              logo
                              selected-bg-effect
                              big-box-sprite
                              )
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
  (when (and (= (event-type event) lgame::+sdl-keydown+)
             (not *scene-ready-to-change*))
    (when (= (key-scancode event) lgame::+sdl-scancode-left+)
      (select-title-item self (aref (.item-names self) (mod (- (.selected-item self) 1) (length (.item-names self))))))
    (when (= (key-scancode event) lgame::+sdl-scancode-right+)
      (select-title-item self (aref (.item-names self) (mod (+ (.selected-item self) 1) (length (.item-names self))))))
    (when (= (key-scancode event) lgame::+sdl-scancode-return+)
      (activate-selection self))
    (when (= (key-scancode event) lgame::+sdl-scancode-escape+)
      (lgame.time:clock-stop))
    ))

(defun activate-selection (self)
  "User has hit enter on a menu item, activate the appropriate scene change."
  (case (.selected-item self)
    (0 (scene-change :play)))
  )

(defmethod scene-update ((self title-scene))
  (update (.sprites self))
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
  (draw (.sprites self)))

(defmethod scene-unload ((self title-scene))
  ;; Transitioning to a new scene results in fading out the title menu options except for the one selected.
  (when (zerop (.unload-frames self))
    (call-next-method)) ; unloaded
  (let ((selected-item-name (aref (.item-names self) (.selected-item self))))
    (loop for item-name across (.item-names self)
          unless (equal selected-item-name item-name)
          do
          (lgame.sprite:set-alpha (gethash item-name (.items self)) (/ (.unload-frames self) (init-unload-frames)))))
  (decf (.unload-frames self)))

;;;; Play scene

(defclass play-scene (scene)
  ((sprites :accessor .sprites :initform (make-instance 'lgame.sprite:ordered-group))
   (hud :accessor .hud)
   ))

(defmethod initialize-instance :after ((self play-scene) &key)
  (lgame.sprite:add-sprites (.sprites self)
                            *starfield*
                            (setf (.hud self) (make-instance 'hud))
                            ))

(defmethod scene-receive-event ((self play-scene) event)
  (when (and (= (event-type event) lgame::+sdl-keydown+)
             (not *scene-ready-to-change*))
    (when (= (key-scancode event) lgame::+sdl-scancode-d+)
      (change-state (.hud self) :draining))
    (when (= (key-scancode event) lgame::+sdl-scancode-escape+)
      (scene-change :title))))

(defmethod scene-update ((self play-scene))
  (update (.sprites self))
  )

(defmethod scene-render ((self play-scene))
  (draw (.sprites self)))


