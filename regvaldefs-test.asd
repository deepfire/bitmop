;;; -*- Mode: Lisp -*-

(defpackage :regvaldefs-test.system
  (:use :cl :asdf))

(in-package :regvaldefs-test.system)

(defsystem :regvaldefs-test
  :depends-on (:regvaldefs :custom-harness)
  :components
  ((:file "test")))
