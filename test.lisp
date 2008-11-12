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

(defpackage #:regvaldefs-test
  (:use :common-lisp :regvaldefs :custom-harness :pergamum :setc)
  (:shadowing-import-from :regvaldefs #:space)
  (:export #:run-tests #:pure-evaluation #:device-related))

(in-package :regvaldefs-test)

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

(deftest pure-evaluation base-bit-expression-test (foo)
  (declare (ignore foo))
  (and (expect-value #b101 (bits (:b0 :b2)))
       (expect-value #b101 (bits (:b0 :b2) (plusp 1) (minusp -1)))))

(deftest pure-evaluation literal-singlet-expression-test (foo)
  (declare (ignore foo))
  (and (expect-value #b1110 (bits (:bc) #b111))
       (expect-value #b1110 (bits (:b1 :b2 :b3)))))

(deftest pure-evaluation literal-compound-expression-test (foo)
  (declare (ignore foo))
  (and (expect-value #b1100 (bits (:b2 :b3)))
       (expect-value #b1100 (bits (:bc) :noo))))

(deftest device-related register-bit-io-test (tdev)
  (setc (devreg tdev :fooreg) 0
        (devbit tdev :fooreg :f0 :write-only t) t)
  (expect-success (and (devbit tdev :fooreg :f0)
                       (not (devbit tdev :fooreg :f1)))))

(deftest device-related compound-numeric-bitvalue-plus-devbit-value-test (tdev)
  (setc (devreg tdev :barreg) 0
        (devbit tdev :barreg :bc) #b110)
  (expect-value (ash (bits (:bc) :noo) -1) (devbit-value tdev :barreg :bc)))

(deftest device-related compound-named-bitvalue-test (tdev)
  (setc (devreg tdev :barreg) 0
        (devbit tdev :barreg :bc) :noo)
  (and (expect-success (not (or (devbit tdev :barreg :b0) (devbit tdev :barreg :b4))))
       (expect-success (and (devbit tdev :barreg :b3) (devbit tdev :barreg :b2) (not (devbit tdev :barreg :b1))))))

(deftest device-related spread-immediate-evaluation-test (tdev)
  (setc (devreg tdev :fooreg) 0
        (devbits tdev :fooreg (:f0 :f1 :f2)) (t nil nil))
  (expect-value '(t nil nil)
                (multiple-value-list (devbits tdev :fooreg (:f0 :f1 :f2)))
                :test 'equal))

(deftest device-related spread-delayed-evaluation-test (tdev)
  (setc (devreg tdev :fooreg) 0
        (devbits tdev :fooreg (:f0 :f1 :f2)) (#b1 (plusp 0) nil))
  (expect-value '(t nil nil)
                (multiple-value-list (devbits tdev :fooreg (:f0 :f1 :f2)))
                :test 'equal))

(deftest device-related compound-value-compositing-test (tdev)
  (setc (devreg tdev :barreg) 0
        (devbits tdev :barreg (:b4 :b0)) (nil nil)
        (devbits tdev :barreg (:bc)) (:noo))
  (expect-value '(nil t t nil nil)
                (multiple-value-list (devbits tdev :barreg (:b4 :b3 :b2 :b1 :b0)))
                :test 'equal))

(deftest device-related multiple-compound-mixed-evaluation-test (tdev)
  (let ((supervar :eoo))
    (setc (devreg tdev :barreg) 0
          (devbits tdev :barreg (:bc)) (supervar))
    (expect-value '(nil t nil nil nil)
                  (multiple-value-list (devbits tdev :barreg (:b4 :b3 :b2 :b1 :b0)))
                  :test 'equal)))

(deftest device-related compound-value-test (tdev)
  (let ((supervar :eoo))
    (setc (devreg tdev :barreg) 0
          (devbits tdev :barreg (:bc)) (supervar))
    (expect-success (test-devbits tdev :barreg :bc :eoo))))

(defun run-tests (&rest test-suites)
  "Run a set of test suites. Defaults to PURE-EVALUATION and DEVICE-RELATED."
  (let ((test-suites (or test-suites '(pure-evaluation device-related))))
    (with-condition-printing (t custom-harness:test-error)
      (lret ((success t))
        (dolist (suite test-suites)
          (andf success (run-test-suite (ecase suite
                                          (pure-evaluation nil)
                                          (device-related (make-instance 'test-device)))
                                        suite)))))))
