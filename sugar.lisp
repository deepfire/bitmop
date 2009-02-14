;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REGVALDEFS; Base: 10 -*-
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

(in-package :regvaldefs)

(defmacro with-device-bits-toggled (device register (&rest bits) (&rest values) &body body)
  "Execute BODY with BITS or DEVICE's REGISTER set to constant boolean VALUES
   before execution and to their negation afterwards."
  (unless (every (of-type 'boolean) values) (error "~@<Values must be boolean constants.~:@>"))
  (once-only (device)
    `(progn
       (setc (devbits ,device ,register ,bits) ,values)
       (unwind-protect (progn ,@body)
         (setc (devbits ,device ,register ,bits) ,(mapcar #'not values))))))
