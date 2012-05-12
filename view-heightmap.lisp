#!/usr/local/bin/sbcl --script
;;;; A very simple png heightmap viewer. Assumes Quicklisp and SBCL -- but
;;;; should be easy to port to other implementations.
;;;;
;;;; By Nikodemus Siivola <nikodemus@random-state.net>, 2012.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :cl-user)

;;; WFT is this, you ask?
;;;
;;; A hack to fail fast without loading deps, and making it tolerable
;;; to run the same code in Emacs and from the commandline.

(defvar *command* nil)

(unless (or *command* (find-package :swank))
  (destructuring-bind (&optional method file) (cdr sb-ext:*posix-argv*)
    (let ((*print-array* nil))
      (setf *command*
            (cond ((string-equal '--x-profile method)
                   `(heightmap-render-profile ,file :axis 0))
                  ((string-equal '--y-profile method)
                   `(heightmap-render-profile ,file :axis 1))
                  ((string-equal '--3d method)
                   `(heightmap-render-3d ,file))
                  ((not file)
                   `(heightmap-render-3d ,method))
                  (t
                   (format *error-output*
                           "./view-heightmap.lisp [--x-profile|--y-profile|--3d] ~
                                                heightmap.png~2%  ~
                          If only the heightmap file is provided, the 3D viewer is used.~%")
                   (sb-ext:quit :unix-status 1 :recklessly-p t)))))))

;;; For reloading

(set-dispatch-macro-character
 #\# #\! (lambda (stream char n)
           (declare (ignore char n))
           (read-line stream)
           (values)))

;;; Dependencies via Quicklisp

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload '(:alexandria :png-read :cl-opengl :cl-glut :cl-glu)))

(defpackage :heightmap
  (:use :cl)
  (:import-from :alexandria #:clamp #:if-let #:when-let)
  (:import-from :cl-user #:heightmap-render-profile #:heightmap-render-3d))

(in-package :heightmap)

(defclass heightmap-window (glut:window)
  ((data :initform nil :accessor heightmap-data)
   (pathname :initform nil :accessor heightmap-pathname)
   (scale :initarg :scale :initform 1.0 :accessor heightmap-scale)
   (reload :initform nil :accessor reload-timestamp)))

(defmethod glut:title ((w heightmap-window))
  (if-let ((pathname (heightmap-pathname w)))
    (file-namestring pathname)
    (format nil "Heightmap ~(~A~)" (class-name (class-of w)))))

(defmethod initialize-instance :before ((window heightmap-window) &key png)
  (with-slots (data pathname) window
    (setf data (png-read:image-data (png-read:read-png-file png))
          pathname (probe-file png))))

(defgeneric set-orthogonal (w)
  (:method ((w heightmap-window))
    (gl:matrix-mode :projection)
    (gl:disable :lighting)
    (gl:load-identity)
    (glu:ortho-2d 0 (glut:width w) 0 (glut:height w))
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defgeneric set-projection (w)
  (:method ((w heightmap-window))
    (gl:matrix-mode :projection)
    (gl:enable :lighting)
    (gl:load-identity)
    (glu:perspective 120 (/ (glut:width w) (glut:height w)) 1 500)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defvar *source-pathname* *load-truename*)

(defgeneric reload (w)
  (:method ((w heightmap-window))
    (multiple-value-bind (fasl warn fail)
        (handler-case
            (compile-file *source-pathname*)
          (error ()
            (values nil t t)))
      (cond ((or warn fail)
             (warn "Compile failed, not reloading."))
            (t
             (load fasl)
             (setf (reload-timestamp w) (get-universal-time)))))))

(defun render-string (control &rest arguments)
  (loop for char across (apply #'format nil control arguments)
        do (glut:bitmap-character glut:+bitmap-helvetica-12+ (char-code char))))

(defun render-status (window control &rest arguments)
  (set-orthogonal window)
  (gl:color 1 0 0)
  (gl:raster-pos 3 3)
  (apply #'render-string control arguments)
  (when-let ((stamp (reload-timestamp window)))
    (multiple-value-bind (sec min hour) (decode-universal-time stamp)
      (render-string "  [reload: ~D:~D:~S]" hour min sec))))

;;;; Scanning a heighmap by lines

(defclass profile-window (heightmap-window)
  ((line :initform 0)
   (axis :initform 0 :initarg :axis :type (integer 0 1)))
  (:default-initargs
   :pos-x 100 :pos-y 100
   :mode '(:single :rgb)))

(defmethod initialize-instance :after ((window profile-window) &key)
  (with-slots (data axis) window
    (setf (glut:height window) 256
          (glut:width window) (array-dimension data axis))))

(defun move-scan-line (window delta)
  (with-slots (line data axis) window
    (setf line (clamp (+ line delta)
                      0
                      (1- (if (zerop axis)
                              (array-dimension data 1)
                              (array-dimension data 0)))))))

(defun adjust-scale (window factor)
  (with-slots (scale) window
    (let ((new (+ factor scale)))
      (when (plusp new)
        (setf scale new)))))

(defmethod glut:display-window :before ((w profile-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat))

(defmethod glut:display ((w profile-window))
  (gl:clear :color-buffer)
  (gl:color 1 1 1)
  (let ((data (heightmap-data w))
        (scale (heightmap-scale w))
        (line (slot-value w 'line))
        (axis (slot-value w 'axis))
        (x0 nil)
        (y0 nil))
    (loop for x1 from 0 below (array-dimension data axis)
          for y1 = (* scale (handler-bind ((error (lambda (c)
                                                    (format t "~A:~%line=~S, x1=~S~%"
                                                            c line x1)
                                                    (sb-ext:quit))))
                                (if (zerop axis)
                                 (aref data x1 line)
                                 (aref data line x1))))
          do (when x0
               (gl:with-primitives :lines
                 (gl:vertex x0 y0)
                 (gl:vertex x1 y1)))
             (setf x0 x1
                   y0 y1))
    (render-status w "Line ~S, Scale ~,1F" line scale))
  (gl:flush))

(defmethod glut:reshape ((w profile-window) width height)
  (gl:viewport 0 0 width height)
  (set-orthogonal w))

(defun heightmap-keypress (w key)
  (case key
    ((#\+ #\= #\?)
     (adjust-scale w 0.1))
    ((#\- #\_)
     (adjust-scale w -0.1))
    ((#\Esc #\q)
     (glut:destroy-current-window))
    (#\r
     (reload w))
    (otherwise
     (format t "~S pressed~%" key)
     nil)))

(defun profile-keypress (w key)
  (when (case key
          (:key-up
           (move-scan-line w 1))
          (:key-down
           (move-scan-line w -1))
          (:key-page-up
           (move-scan-line w 10))
          (:key-page-down
           (move-scan-line w -10))
          (otherwise
           (heightmap-keypress w key)))
    (glut:post-redisplay)))

(defmethod glut:keyboard ((w profile-window) key x y)
  (declare (ignore x y))
  (profile-keypress w key))

(defmethod glut:special ((w profile-window) key x y)
  (declare (ignore x y))
  (profile-keypress w key))

(defun heightmap-render-profile (pathname &key (axis 0))
  (glut:display-window
   (make-instance 'profile-window
                  :png pathname
                  :axis axis)))

;;;; 3D rendering of a heightmap

(defclass 3d-window (heightmap-window)
  ((point-list :initform nil :accessor point-list)
   (face-list :initform nil :accessor face-list)
   (look-at :initform (list 0 0) :accessor look-at)
   (camera :initform (list 0 0) :accessor camera)
   (show-faces-p :initform t :accessor show-faces-p))
  (:default-initargs
   :width 600 :height 480
   :pos-x 100 :pos-y 100
   :mode '(:single :rgb :depth)
   :scale 0.1))

(defun delete-lists (window)
  (with-slots (point-list face-list) window
    (let ((points point-list)
          (faces face-list))
      (setf point-list nil
            face-list nil)
      (when points
        (gl:delete-lists points 1))
      (when faces
        (gl:delete-lists faces 1)))))

(defmethod reload :after ((w 3d-window))
  (delete-lists w))

(defmethod point-list :before ((w 3d-window))
  (with-slots (point-list) w
    (unless point-list
      (setf point-list (gl:gen-lists 1))
      (gl:with-new-list (point-list :compile)
        (gl:with-primitives :points
          (let ((data (heightmap-data w)))
            (loop for x from 0 below (array-dimension data 0)
                  do (loop for y from 0 below (array-dimension data 1)
                           do (gl:vertex x y (aref data x y))))))))))

(defmethod face-list :before ((w 3d-window))
  (with-slots (face-list) w
    (unless face-list
      (setf face-list (gl:gen-lists 1))
      (gl:with-new-list (face-list :compile)
        (gl:material :front :ambient #(0.05 0.05 0.05 0))
        (gl:material :front :diffuse #(0.8 0.8 0.8 0))
        (let ((data (heightmap-data w)))
          (loop for x from 0 below (1- (array-dimension data 0))
                do (gl:with-primitives :quad-strip
                     (loop for y from 0 below (array-dimension data 1)
                           do (make-vertex x y data)
                              (make-vertex (1+ x) y data)))))))))

(declaim (inline normalized-cross-product))
(defun normalized-cross-product (a1 a2 a3 b1 b2 b3)
  (declare (single-float a1 a2 a3 b1 b2 b3))
  (let* ((c1 (- (* a2 b3) (* a3 b2)))
         (c2 (- (* a3 b1) (* a1 b3)))
         (c3 (- (* a1 b2) (* a2 b1)))
         (d (/ 1.0 (sqrt (+ (* c1 c1) (* c2 c2) (* c3 c3))))))
    (values (* c1 d) (* c2 d) (* c3 d))))

(defun make-vertex (x0 y0 data)
  (let* ((z0 (aref data x0 y0))
         (xs (if (array-in-bounds-p data (1+ x0) y0)
                 1
                 -1))
         (x1 (+ x0 xs))
         (ys (if (array-in-bounds-p data x0 (1+ y0))
                 1
                 -1))
         (y1 (+ y0 ys))
         (zx (aref data x1 y0))
         (zy (aref data x0 y1)))
    (multiple-value-bind (x y z)
        (normalized-cross-product (coerce xs 'single-float)
                                  0.0
                                  (coerce (- zx z0) 'single-float)
                                  0.0
                                  (coerce ys 'single-float)
                                  (coerce (- zy z0) 'single-float))
      (gl:normal x y z)
      (gl:vertex x0 y0 z0))))

(defmethod initialize-instance :after ((w 3d-window) &key)
  (with-slots (data scale camera look-at) w
    (setf camera (list (round (array-dimension data 0) 2)
                       0)
          look-at (list (elt camera 0)
                        100))))

(defmethod glut:display-window :before ((w 3d-window))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :smooth)
  (gl:enable :lighting :light0)
  (gl:enable :depth-test :cull-face))

(defmethod glut:display ((w 3d-window))
  (set-projection w)
  (gl:shade-model :smooth)
  (gl:clear :color-buffer :depth-buffer)
  (gl:color 1 1 1)
  (gl:light :light0 :ambient #(1 1 1 0))
  (gl:light :light0 :position #(1 1 10 0))
  (let ((data (heightmap-data w))
        (s (heightmap-scale w)))
    (destructuring-bind (lx ly) (look-at w)
      (destructuring-bind (cx cy) (camera w)
        (glu:look-at cx cy (* s (+ 255
                                   (if (array-in-bounds-p data cx cy)
                                       (aref data cx cy)
                                       0)))
                     lx ly 0
                     0 0 1)))
    (gl:scale 1 1 s)
    (gl:call-list (if (show-faces-p w)
                      (face-list w)
                      (point-list w)))
    (render-status w "Scale ~,2F: 0 - ~,2F" s (* 255.0 s)))
  (gl:flush))

(defmethod glut:reshape ((w 3d-window) width height)
  (gl:viewport 0 0 width height)
  (set-projection w))

(defun 3d-keypress (w key)
  (let ((speed 2))
    (when (case key
            (:key-right
             (incf (elt (camera w) 0) speed)
             (incf (elt (look-at w) 0) speed))
            (:key-left
             (decf (elt (camera w) 0) speed)
             (decf (elt (look-at w) 0) speed))
            (:key-up
             (incf (elt (camera w) 1) speed)
             (incf (elt (look-at w) 1) speed))
            (:key-down
             (decf (elt (camera w) 1) speed)
             (decf (elt (look-at w) 1) speed))
            (#\Newline
             (rotatef (camera w) (render-look-at w))
             t)
            (#\Space
             (setf (show-faces-p w) (not (show-faces-p w)))
             t)
            (otherwise
             (heightmap-keypress w key)))
      (glut:post-redisplay))))

(defmethod glut:keyboard ((w 3d-window) key x y)
  (declare (ignore x y))
  (3d-keypress w key))

(defmethod glut:special ((w 3d-window) key x y)
  (declare (ignore x y))
  (3d-keypress w key))

(defun heightmap-render-3d (pathname)
  (let ((window (make-instance '3d-window :png pathname)))
    ;; KLUDGE: CL-OPENGL doesn't seem to give us access to
    ;; a hook on DESTROY-WINDOW.
    (unwind-protect
         (glut:display-window window)
      (delete-lists window))))

(in-package :cl-user)

;;; Finally run the thing.
(let ((command *command*))
  (setf *command* t)
  (eval command))
