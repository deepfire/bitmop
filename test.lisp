;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REGVALDEFS; Base: 10 -*-
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

(in-package :regvaldefs)

(define-namespace :foo
  (:implemented-by nil)
  (:documentation "Foo device")
  (:register-formats
   (:fooreg "Foo-type register"
	    (:f0		1 0 "Memory Space")
	    (:f1		1 1 "Bus Master")
	    (:f2		1 2 "Parity Error Response")))
  (:register-layouts
   ((:foo "foo register layout")
    (:fooreg		0 :format :fooreg :doc "foo register 0"))))

(define-namespace :bar
  (:implemented-by nil)
  (:documentation "Bar device")
  (:register-formats
   (:barreg "Bar-type register"
	    (:bc		3 1 ""
				((#b110	:noo	"fooance")
				 (#b000	:doo	"booance")
				 (#b100	:eoo	"mooance")))
	    (:b0		1 0 "")
	    (:b1		1 1 "")
	    (:b2		1 2 "")
	    (:b3		1 3 "")
	    (:b4		1 4 "")))
  (:register-layouts
   ((:bar "bar register layout")
    (:barreg		0 :format :barreg :doc "bar register 0"))))

(defclass test-device (device)
  ((hash :accessor test-device-hash :initform (make-hash-table)))
  (:default-initargs
   :space (space :foo)))

(defun testreg (device selector)
  (declare (type test-device device))
  (gethash selector (test-device-hash device)))

(defun (setf testreg) (val device selector)
  (declare (type test-device device))
  (setf (gethash selector (test-device-hash device)) val))

(set-namespace :foo :bar)

(define-register-set-accessor :foo :foo 'testreg "foo regset")
(define-register-set-accessor :bar :bar 'testreg "bar regset")

(defparameter tdev (make-instance 'test-device))

(unless (= (bits (:b0 :b2)) #b101 (bits (:b0 :b2) (plusp 1) (minusp -1)))
  (error "BITS base test failed."))

(unless (= (bits (:bc) #b111) (bits (:b1 :b2 :b3)))
  (error "BITS literal value test failed: ~B vs ~B."
         (bits (:bc) #b111) (bits (:b1 :b2 :b3))))

(unless (= (bits (:bc) :noo) (bits (:b2 :b3)))
  (error "BITS named value test failed: ~B vs. ~B."
         (bits (:bc) :noo) (bits (:b2 :b3))))

(unless (= #b110 (bit-value (bits (:bc) :noo) :bc))
  (error "BIT-VALUE/BITS compound test failed: ~B vs. ~B."
         (bit-value (bits (:bc) :noo) :bc) #b110))

(setc (devreg tdev :fooreg) 0 (devreg tdev :barreg) 0)
(setc (devbit tdev :fooreg :f0 :write-only t) t)
(unless (and (devbit tdev :fooreg :f0) (not (devbit tdev :fooreg :f1)))
  (error "(SETC DEVBIT)/DEVBIT single boolean bit test failed."))

(setc (devreg tdev :barreg) 0)
(setc (devbit tdev :barreg :bc) #b110)
(unless (= (devbit-value tdev :barreg :bc) (ash (bits (:bc) :noo) -1))
  (error "(SETC DEVBIT)/DEVBIT single numeric bit test failed: ~B instead of ~B."
         (devbit-value tdev :barreg :bc) (ash (bits (:bc) :noo) -1)))

(setc (devreg tdev :barreg) 0)
(setc (devbits tdev :barreg (:bc)) (#b110))
(unless (= (devbit-value tdev :barreg :bc) (ash (bits (:bc) :noo) -1))
  (error "(SETC DEVBITS)/DEVBITS single numeric bit test failed: ~B instead of ~B."
         (devbit-value tdev :barreg :bc) (ash (bits (:bc) :noo) -1)))

(setc (devreg tdev :barreg) 0)
(setc (devbit tdev :barreg :bc) :noo)
(unless (= (devbit-value tdev :barreg :bc) (ash (bits (:bc) :noo) -1))
  (error "coumpound named bit + DEVBIT-VALUE test failed: full:~B/~B instead of ~B."
         (devreg tdev :barreg) (devbit-value tdev :barreg :bc) (ash (bits (:bc) :noo) -1)))

(setc (devreg tdev :barreg) 0)
(setc (devbit tdev :barreg :bc) :noo)
(unless (and (not (devbit tdev :barreg :b4))
             (devbit tdev :barreg :b3) (devbit tdev :barreg :b2) (not (devbit tdev :barreg :b1))
             (not (devbit tdev :barreg :b0)))
  (error "(SETC DEVBIT)/DEVBIT compound bitfield named value test failed: read ~B."
         (devreg tdev :barreg)))

(setc (devreg tdev :fooreg) 0)
(let ((write '(t nil nil)))
  (setc (devbits tdev :fooreg (:f0 :f1 :f2)) (t nil nil))
  (let ((read (multiple-value-list (devbits tdev :fooreg (:f0 :f1 :f2)))))
    (unless (equal read write)
      (error "(SETC DEVBITS)/DEVBITS spread bits immediate test failed: wrote ~S read ~S."
             write read))))

(setc (devreg tdev :fooreg) 0)
(let ((write '(t nil nil)))
  (setc (devbits tdev :fooreg (:f0 :f1 :f2)) (#b1 (plusp 0) nil))
  (let ((read (multiple-value-list (devbits tdev :fooreg (:f0 :f1 :f2)))))
    (unless (equal read write)
      (error "(SETF DEVBITS)/DEVBITS spread bits delayed evaluation test failed."))))

(setc (devreg tdev :barreg) 0)
(let ((write '(nil t t nil nil)))
  (setc (devbits tdev :barreg (:b4 :b0)) (nil nil)
        (devbits tdev :barreg (:bc)) (:noo))
  (let ((read (multiple-value-list (devbits tdev :barreg (:b4 :b3 :b2 :b1 :b0)))))
    (unless (equal read write)
      (error "(SETF DEVBITS)/DEVBITS compound value compositing test failed: read ~B -> ~S."
             (devreg tdev :barreg) read))))

(setc (devreg tdev :barreg) 0)
(let ((write '(nil t nil nil nil))
      (suprvar :eoo))
  (setc (devbits tdev :barreg (:bc)) (suprvar))
  (let ((read (multiple-value-list (devbits tdev :barreg (:b4 :b3 :b2 :b1 :b0)))))
    (unless (equal read write)
      (error "(SETF DEVBITS)/DEVBITS multiple compound value mixed-evaluation test failed: read ~B -> ~S."
             (devreg tdev :barreg) read))))

(setc (devreg tdev :barreg) 0)
(let ((suprvar :eoo))
  (setc (devbits tdev :barreg (:bc)) (suprvar))
  (unless (test-devbits tdev :barreg :bc :eoo)
    (error "TEST-DEVBITS compound value test failed: read ~B."
           (devreg tdev :barreg))))

(undefine-space :foo)
(undefine-space :bar)
(undefine-space '(:foo :bar)) ;; FIXME
