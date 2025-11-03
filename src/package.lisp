(defpackage #:solardragon
  (:use #:cl)
  (:export #:main)
  (:import-from #:lgame.loader
                #:get-texture)
  (:import-from #:lgame.sprite
                #:.image
                #:.box)
  (:import-from #:lgame.box
                #:box-attr
                #:get-texture-box)
  (:import-from #:lgame.event
                #:event-type
                #:key-scancode))

