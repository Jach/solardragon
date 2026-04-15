(in-package #:solardragon)

(defclass level ()
  ((number :accessor .number :initarg :number)
   (title :accessor .title :initarg :title)
   (secondary-title :accessor .secondary-title :initarg :secondary-title)
   (start-row :accessor .start-row :initarg :start-row)
   (start-col :accessor .start-col :initarg :start-col)
   (cube-locations :accessor .cube-locations :initarg :cube-locations)))

(defmethod print-object ((lvl level) stream)
  (print-unreadable-object (lvl stream :type t :identity t)
    (format stream "Number:~a Title:~s Secondary-Title:~s Start:[~a ~a] Cubes:~a"
            (.number lvl) (.title lvl) (.secondary-title lvl) (.start-row lvl) (.start-col lvl) (.cube-locations lvl))))

(defvar *level-data* (make-array 0 :element-type 'level :adjustable t :fill-pointer 0)
  "Growable array of all level data, loaded from levels.txt.")

(defun level-char-to-object (char)
  "Convert a character from level data to a cube type keyword."
  (case char
    (#\# :cube)
    (#\* :super-cube) ; needs to be collected more than once, basically
    (#\s :start)
    (#\space :empty)
    (t :empty)))

(defun parse-levels-file (filename)
  "Parse levels.txt and populate *level-data* with level objects.

   Each level is split by ! separators. Each level contains:
   - Lines starting with > for the title
   - Lines starting with < for the secondary title
   - The (max) 9x7 grid with #, *, s, and space characters

   The cube-locations slot is a hash table keyed by #(row col) vectors,
   each entry mapping to :cube or :super-cube.
   "
  (let* ((contents (uiop:read-file-string filename))
         (levels (cl-ppcre:split "\\n!\\n" contents)))
    (loop for level in levels
          for level-number from 0
          do
          (let* ((lines (uiop:split-string level :separator '(#\newline)))
                 (title "")
                 (secondary-title "")
                 (start-row 0)
                 (start-col 0)
                 (cube-locations (make-hash-table :test #'equalp)))
            ;; Process each line up to max 7 rows
            (loop with row = 0
                  for line in lines
                  for line-len = (length line)
                  do
                  (cond
                    ;; Title line
                    ((and (> line-len 0)
                          (eql (char line 0) #\>))
                     (setf title (subseq line 2)))
                    ;; Secondary title line
                    ((and (> line-len 0)
                          (eql (char line 0) #\<))
                     (setf secondary-title (subseq line 2)))
                    ;; Comment
                    ((and (> line-len 0)
                          (eql (char line 0) #\;))
                     nil)
                    (t
                     ;; Parse grid col up to max 9 cols
                     (loop for char across line
                           for col from 0 upto 9
                           while (< col 9)
                           do
                           (let ((obj (level-char-to-object char)))
                             (when (member obj '(:cube :super-cube))
                               (setf (gethash (vector row col) cube-locations) obj))
                             (when (eql obj :start)
                               (setf start-row row
                                     start-col col))))
                     (incf row))
                    ))
            ;; Finished level object
            (vector-push-extend
              (make-instance 'level
                             :number level-number
                             :title title
                             :secondary-title secondary-title
                             :start-row start-row
                             :start-col start-col
                             :cube-locations cube-locations)
              *level-data*)))))

(defun get-cube-at (level rowcol)
  "Get the cube object at a given rowcol vector on a level."
  (gethash rowcol (.cube-locations level)))

(defun load-levels ()
  (setf *level-data* (make-array 0 :element-type 'level :adjustable t :fill-pointer 0))
  (let ((file (merge-pathnames "levels.txt" (assets-dir))))
    (parse-levels-file file)))

