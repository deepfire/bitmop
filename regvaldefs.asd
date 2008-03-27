(defpackage :regvaldefs.system
  (:use :cl :asdf))

(in-package :regvaldefs.system)

(defsystem :regvaldefs
  :depends-on (:alexandria :iterate :pergamum)
  :components
  ((:file "setc")
   (:file "regvaldefs" :depends-on ("setc"))))
