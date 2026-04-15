(in-package #:solardragon)

;; Toast messages for level titles and other info.

(defclass message (lgame.sprite:sprite)
  ((text :accessor .text :initarg :text :initform "")))
