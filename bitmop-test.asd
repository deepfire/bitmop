;;; -*- Mode: Lisp -*-

(defpackage :bitmop-test.system
  (:use :cl :asdf))

(in-package :bitmop-test.system)

(defsystem :bitmop-test
  :depends-on (:bitmop :custom-harness)
  :components
  ((:file "test")))
