(in-package #:solardragon)

(defparameter *game-width* 800)
(defparameter *game-height* 600)
(defvar *arena-box* (lgame.box:make-box 55 50 590 490))

(defparameter *start-lives* 4)

(defconstant +frame-adjust+ (/ 40.0 60.0)) ; since original ran at 40fps and we run at 60, sometimes need to multiply by this constant...

(defvar *debug* nil)
