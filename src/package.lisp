(defpackage #:solardragon
  (:use #:cl)
  (:export #:main)
  (:local-nicknames (#:fc #:com.thejach.frame-chronicle))
  (:import-from #:lgame.loader
                #:get-texture)
  (:import-from #:lgame.sprite
                #:.image
                #:.box
                #:update
                #:draw
                #:kill)
  (:import-from #:lgame.box
                #:box-attr
                #:get-texture-box
                #:box-width
                #:box-x
                #:move-box)
  (:import-from #:lgame.event
                #:event-type
                #:key-scancode)
  (:import-from #:alexandria
                #:when-let
                #:if-let))

