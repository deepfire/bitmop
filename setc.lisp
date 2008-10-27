;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SETC; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2008 by
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

(defpackage setc
  (:use :common-lisp :alexandria :pergamum)
  (:documentation
   "Defines a controlled version of SETF, aka SETC, which is given full control over
    evaluation of both the right-hand and left-hand sides of the place-setting form.")
  (:export
   #:define-setc-expander #:get-setc-expansion #:setc))

(in-package :setc)

(defvar *setc-expanders* (make-hash-table))

(defmacro define-setc-expander (name lambda-list &body body)
  "Defines a drop-in replacement for SETF which is given full control over
   evaluation of both the right-hand and left-hand sides of the place-setting form."
  (let ((expander-name (format-symbol (symbol-package name) "SETC-~A" (string name))))
    (when (gethash name *setc-expanders*)
      (simple-style-warning "redefining ~S in DEFINE-SETC-EXPANDER" name))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmacro ,expander-name ,lambda-list ,@body)
       (setf (gethash ',name *setc-expanders*) ',expander-name))))

(defun get-setc-expansion (place value)
  "Returns the form which is to be used for expanding a particular SETC." 
  (or (and (consp place)
	   (destructuring-bind (name . rest) place
	     (let ((setc-expander-name (gethash name *setc-expanders*)))
	       (when setc-expander-name
		 `(,setc-expander-name ,value ,@rest)))))
      `(setf ,place ,value)))

(defmacro setc (&body body)
  "A drop-in replacement for SETF which is given full control over
   evaluation of both the right-hand and left-hand sides of the place-setting form."
  `(progn
     ,@(loop :for (place value . rest) :on body :by #'cddr
	     :collect (get-setc-expansion place value))))


