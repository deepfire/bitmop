;;; -*- Mode: Lisp -*-

(defpackage :regvaldefs.system
  (:use :cl :asdf))

(in-package :regvaldefs.system)

(defsystem :regvaldefs
  :depends-on (:alexandria :iterate :pergamum
               :semi-precious) ;; for dictionaries and early-eval
  :components
  ((:file "setc")
   (:file "package" :depends-on ("setc"))
   (:file "utils" :depends-on ("package"))
   (:file "regvaldefs" :depends-on ("utils"))
   (:file "sugar" :depends-on ("regvaldefs"))))
