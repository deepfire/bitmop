;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BITMOP; Base: 10 -*-
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

(in-package :bitmop)

(defun prepend/reduce-equiv (what fn to &key identity (test #'eql))
  "Reduce WHAT with FN, then APPEND whatever is produced with TO, unless
     that which is produced is an identity transform of identity,
     with regard to test."
  (let ((appendee (reduce fn what :initial-value identity)))
    (if (funcall test appendee identity)
        to
        (append (list appendee) to))))

(defun eval-if (predicate form)
  "XXX: rename to MAYBE-EVAL?"
  (if predicate
      (eval form)
      form))
