(in-package #:solardragon)

(defun assets-dir ()
  (asdf:system-relative-pathname "solardragon" "assets/"))

(defun main ()
  (lgame:init)

  (lgame.display:create-window "SolarDragon"
                               *game-width* *game-height*)
  (lgame.display:create-renderer)
  (sdl2:hide-cursor)
  (sdl2:pump-events)

  (lgame.display:set-logical-size *game-width* *game-height*)

  (lgame.loader:create-texture-loader (assets-dir))

  (scene-change :title)

  (lgame.time:clock-start)
  (unwind-protect
    (loop while (lgame.time:clock-running?) do
          (livesupport:continuable
            (game-tick)))

    (quit)))

(defun quit ()
  (lgame:quit))

(defun game-tick ()
  (lgame.event:do-event (event)
    (when (= (event-type event) lgame::+sdl-quit+)
      (lgame.time:clock-stop))

    (scene-receive-event *current-scene* event))

  (scene-update *current-scene*)

  (lgame.render:clear)
  (scene-render *current-scene*)
  (lgame.render:present)

  (livesupport:update-repl-link)
  (lgame.time:clock-tick 60))

(eval-when (:execute)
  (main))
