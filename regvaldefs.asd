(defpackage :regvaldefs.system
  (:use :cl :asdf))

(in-package :regvaldefs.system)

(defsystem :regvaldefs
  :depends-on (:alexandria :iterate :pergamum)
  :components
  ((:file "setc")
   (:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "regvaldefs" :depends-on ("setc" "utils"))
   (:file "test" :depends-on ("regvaldefs"))))
