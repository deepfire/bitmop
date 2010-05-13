;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DEVMODEL; Base: 10; indent-tabs-mode: nil -*-
;;;
;;;  (c) copyright 2010, ГУП НПЦ "Элвис"
;;;
;;;  (c) copyright 2010 by
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

(in-package :devmodel)


(defvar *device-classes* (make-hash-table :test 'eq))

(defparameter *dummy-fixnum-vector* (make-array 0 :element-type 'fixnum))
(defparameter *dummy-function-vector* (make-array 0 :element-type 'function))
(defparameter *dummy-simple-array-vector* (make-array 0 :element-type 'simple-array))
(defparameter *dummy-space* (make-instance 'space :name :dummy :documentation "Dummy space."))

(defstruct (device-class (:include docunamed) (:conc-name device-class-))
  (parents nil :type list)
  (selectors *dummy-fixnum-vector* :type (vector fixnum))
  (readers *dummy-function-vector* :type (vector function))
  (writers *dummy-function-vector* :type (vector function))
  (space *dummy-space* :type space)
  (layouts nil :type list)
  (enumeration-class nil :type symbol)
  (direct-layout-specs nil :type list)
  (effective-layout-specs nil :type list))

(defstruct (struct-device-class (:include device-class))
  (constructor nil #-ccl :type #-ccl (function (*) device-class)))

(defstruct (extended-register-device-class (:include device-class) (:conc-name device-class-))
  (extended-layouts nil :type list)
  (extensions *dummy-simple-array-vector* :type (vector simple-array)))

(define-root-container *device-classes* device-class :type device-class :coercer t :iterator do-device-classes :if-exists :continue)