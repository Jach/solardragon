(in-package #:solardragon)

;; Implementation of a simple signals / triggers / events system to try for this project. May eventually migrate to lgame. These are unrelated to CL conditions and the cl:signal function.

;; Each signal is a keyword :id and an optional datum.
;; Any part of the code can signal at any time, and any part of the code can check for a signal at any time. There's no pub-sub model here.

;; A signal by default has a lifetime of :read-always. Once signaled, other code can check and see that the signal has been made, and that will be true for the life of the program. (Or until clear-signals is called.)
;; If another signal with the same id and :read-always lifetime is sent, the original signal's datum is replaced with the new datum.

;; An alternative lifetime is :read-once. Once signaled, the first time other code checks for the signal and sees it has been sent, the signal is erased and further checks will resolve as if the signal has not yet been sent.
;; Signals with this lifetime form a thread-safe queue so that no particularly care must be made to only send after being sure something has been received.

;; If a :read-always signal exists and a :read-once is sent, nothing happens, the new signal is dropped.
;; If a :read-once signal exists and a :read-always signal is sent, again nothing happens, the new signal is dropped.

;; When checking for a signal, if it has been sent, then either the datum or T will be returned, otherwise nil.

;; The main benefit to this approach is looser coupling of code components that want to signal or wait on other signals. The potential downside is signal names are just keywords, so code could potentially wait forever in the face of typos or other errors and it can be harder to debug than simple missing function calls.

(defvar *signal-holder* (make-hash-table))
(defvar *signal-holder-lock* (bt:make-lock))

(defun clear-signals ()
  (clrhash *signal-holder*))

(defstruct game-signal
  id
  datum
  lifetime)

(defun send-signal (signal-id &key datum (lifetime :read-always))
  (bt:with-lock-held (*signal-holder-lock*)
    (multiple-value-bind (entry found?) (gethash signal-id *signal-holder*)
      (if found?
          (cond
            ((and (eql lifetime :read-always)
                  (atom entry)
                  (eql lifetime (game-signal-lifetime entry)))
             (setf (game-signal-datum entry) datum))

            ((and (eql lifetime :read-once)
                  (listp entry)
                  (eql lifetime (game-signal-lifetime (first entry))))
             (nconc entry (list (make-game-signal :id signal-id :datum datum :lifetime lifetime)))))

          (let ((new-signal (make-game-signal :id signal-id :datum datum :lifetime lifetime)))
            (ecase lifetime
              (:read-always (setf (gethash signal-id *signal-holder*) new-signal))
              (:read-once (setf (gethash signal-id *signal-holder*) (list new-signal)))))))))

(defun check-signal (signal-id)
  (bt:with-lock-held (*signal-holder-lock*)
    (multiple-value-bind (entry found?) (gethash signal-id *signal-holder*)
      (when found?
        (if (atom entry)
            (or (game-signal-datum entry) t)
            (let ((val (or (game-signal-datum (first entry)) t))
                  (next (rest entry)))
              (if next
                  (setf (gethash signal-id *signal-holder*) next)
                  (remhash signal-id *signal-holder*))
              val))))))

