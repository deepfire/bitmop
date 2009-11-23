;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BITMOP; Base: 10 -*-
;;;
;;;  (c) copyright 2009 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :device-model)

(defmacro with-device-bits-toggled (device register (&rest bits) (&rest values) &body body)
  "Execute BODY with BITS or DEVICE's REGISTER set to constant boolean VALUES
   before execution and to their negation afterwards."
  (unless (every (of-type 'boolean) values) (error "~@<Values must be boolean constants.~:@>"))
  (once-only (device)
    `(progn
       (setc (devbits ,device ,register ,bits) ,values)
       (unwind-protect (progn ,@body)
         (setc (devbits ,device ,register ,bits) ,(mapcar #'not values))))))

(defun iolog-wrap-reader (stream space format-control reader register-id)
  (lambda (device selector)
    (lret ((ret (funcall reader device selector)))
      (format stream format-control (name (register-by-id space register-id)) ret))))

(defun iolog-wrap-writer (stream space format-control writer register-id)
  (lambda (value device selector)
    (format stream format-control value (name (register-by-id space register-id)))
    (funcall writer value device selector)))

(defun iolog-wrap-device-accessors (stream device)
  (declare (optimize debug safety))
  (let* ((orig-readers (device-readers device))
         (orig-writers (device-writers device))
         (class (class-of-device device))
         (space (device-class-space class))
         (register-name-lengths (mapcar (compose #'length #'string #'name) (mappend #'layout-registers (device-class-layouts class)))))
    (unless register-name-lengths
      (bit-notation-error "~@<Asked to trace I/O activity of a ~S, which has no registers.~:@>" device))
    (let* ((reader-format (cl:format nil "~~~D,0T ~~A~~~D,0T -> ~~X~~%" 11 (+ 10 (apply #'max register-name-lengths))))
           (writer-format (cl:format nil "~~8,' X -> ~~A~~%"))
           (iologging-readers (map '(vector function) (curry #'iolog-wrap-reader stream space reader-format) orig-readers (iota (length orig-readers))))
           (iologging-writers (map '(vector function) (curry #'iolog-wrap-writer stream space writer-format) orig-writers (iota (length orig-writers)))))
      (setf (device-readers device) iologging-readers
            (device-writers device) iologging-writers))
    (values orig-readers orig-writers)))

(defmacro with-logged-device-io ((device stream) &body body)
  "Execute BODY with all register access of DEVICE-CLASS logged to STREAM."
  (with-gensyms (orig-readers orig-writers)
    (once-only (device)
      `(multiple-value-bind (,orig-readers ,orig-writers) (iolog-wrap-device-accessors ,stream ,device)
         (unwind-protect (progn ,@body)
           (setf (device-readers ,device) ,orig-readers
                 (device-writers ,device) ,orig-writers))))))

(defun execute-with-maybe-logged-device-io (fn device stream log-p)
  "Execute FN with all register access of DEVICE-CLASS conditionally logged
to STREAM, depending on whether LOG-P evaluates to a non-NIL value."
  (if log-p
      (with-logged-device-io (device stream)
        (funcall fn))
      (funcall fn)))

(defmacro with-maybe-logged-device-io ((device stream log-p) &body body)
  "Execute BODY with all register access of DEVICE-CLASS conditionally logged
to STREAM, depending on whether LOG-P evaluates to a non-NIL value."
  `(execute-with-maybe-logged-device-io (lambda () ,@body) ,device ,stream ,log-p))