(in-package #:solardragon)

(defclass animation-ticker ()
  ((ticks :accessor .ticks :initform 0
          :documentation "Represents how many frames / update-ticks have passed.")
   (elapsed :accessor .elapsed :initform 0.0
            :documentation "Represents cumulative time, can be easier to reason with than frame ticks. Increases by lgame.time:dt once per frame."))
  (:documentation
    "Ticker class provides ticks and elapsed slots for child objects, useful for sprite animation. Will probably go in lgame if I like it enough."))

(defmethod tick ((self animation-ticker))
  (incf (.ticks self))
  (incf (.elapsed self) (lgame.time:dt)))

(defmethod reset-ticks ((self animation-ticker))
  (setf (.ticks self) 0
        (.elapsed self) 0.0))

