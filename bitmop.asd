;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

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
   (:file "device-class" :depends-on ("bitmop"))
   ;;
   (:file "device-model" :depends-on ("device-class"))
   ;;
   (:file "sugar" :depends-on ("device-model"))))
