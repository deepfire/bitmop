;;; -*- Mode: Lisp -*-

(defpackage :bitmop.system
  (:use :cl :asdf))

(in-package :bitmop.system)

(defsystem :bitmop
  :depends-on (:alexandria :iterate :pergamum :semi-precious)
  :components
  ((:file "setc")
   ;;
   (:file "package" :depends-on ("setc"))
   ;;
   (:file "utils" :depends-on ("package"))
   ;;
   (:file "bitmop" :depends-on ("utils"))
   ;;
   (:file "device-model" :depends-on ("bitmop"))
   ;;
   (:file "sugar" :depends-on ("device-model"))))
