(defpackage setc
  (:use :common-lisp :alexandria :pergamum)
  (:documentation
   "Defines a controlled version of SETF, aka SETC, which is given full control over
    evaluation of both the right-hand and left-hand sides of the place-setting form.")
  (:export
   #:define-setc-expander #:get-setc-expansion #:setc))

(in-package :setc)

(defvar *setc-expanders* (make-hash-table))

(defmacro define-setc-expander (name lambda-list &body body)
  "Defines a drop-in replacement for SETF which is given full control over
   evaluation of both the right-hand and left-hand sides of the place-setting form."
  (let ((expander-name (format-symbol (symbol-package name) "SETC-~A" (string name))))
    (when (gethash name *setc-expanders*)
      (simple-style-warning "redefining ~S in DEFINE-SETC-EXPANDER" name))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmacro ,expander-name ,lambda-list ,@body)
       (setf (gethash ',name *setc-expanders*) ',expander-name))))

(defun get-setc-expansion (place value)
  "Returns the form which is to be used for expanding a particular SETC." 
  (or (and (consp place)
	   (destructuring-bind (name . rest) place
	     (let ((setc-expander-name (gethash name *setc-expanders*)))
	       (when setc-expander-name
		 `(,setc-expander-name ,value ,@rest)))))
      `(setf ,place ,value)))

(defmacro setc (&body body)
  "A drop-in replacement for SETF which is given full control over
   evaluation of both the right-hand and left-hand sides of the place-setting form."
  `(progn
     ,@(loop :for (place value . rest) :on body :by #'cddr
	     :collect (get-setc-expansion place value))))


