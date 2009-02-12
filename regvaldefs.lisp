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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-reader)
  (enable-compose-reader))

(defvar *log-stream* *standard-output*)
(defvar *verbose-device-init-p* t)

(defclass space ()
  ((name :accessor space-name :type (or keyword list) :initarg :name)
   (documentation :accessor space-documentation :type string :initarg :documentation)
   (referrers :accessor space-referrers :initform nil :type list)

   (devices :accessor devices :initform (make-hash-table :test 'equal) :type hash-table)
   (layouts :accessor layouts :initform (make-hash-table) :type hash-table)
   (registers :accessor register-dictionary :initform (make-dictionary) :type dictionary)
   (bitfields :accessor bitfields :initform (make-hash-table) :type hash-table)
   (bitfield-bytes :accessor bitfield-bytes :initform (make-hash-table) :type hash-table)
   ))

(defmethod print-object ((space space) stream)
  (cl:format stream "~@<#<SPACE:~;~A ~S registers: ~S devices: ~S layouts: ~S~;>~:@>"
             (space-name space)
             (space-documentation space)
             (map 'list #'name (dictionary::dictionary-ids-to-values (register-dictionary space)))
             (maphash-values #'device-hash-id (devices space))
             (maphash-values #'name (layouts space))))

(defstruct (docunamed (:conc-name nil))
  (name nil :type symbol)
  (documentation "" :type string))

(defstruct (spaced (:include docunamed))
  (space nil :type (or null space)))

(defstruct (format (:include spaced))
  bitfields)

(defstruct (bitfield (:include spaced))
  format%
  spec
  (bytevals (make-hash-table) :type hash-table)
  (byterevvals (make-hash-table) :type hash-table))

(defstruct (layout (:include spaced) (:constructor %make-layout))
  "Maps register names into register structures."
  name-format
  registers
  register-selectors)

(defun make-layout (&rest args &key name-format &allow-other-keys)
  (apply #'%make-layout :name-format (or name-format (cl:format nil "~~A~~^"))
	 (remove-from-plist args :name-format)))

(defstruct (register (:include spaced) (:conc-name reg-))
  "Defines a formatted register, specified within a layout with a selector."
  aliases
  layout
  format
  type ext)

(defstruct (register-instance (:include docunamed) (:conc-name reginstance-))
  "Instance of register."
  device
  register
  selector
  reader writer
  id)
  
(defstruct (byteval (:include spaced))
  byte
  value)

(defun byte-bitmask (byte &optional (acc 0))
  (dpb -1 byte acc))

(defun bytes-bitmask (bytes)
  (reduce #'byte-bitmask bytes :initial-value 0 :from-end t))

(defun flatten-byteval (byteval)
  (dpb (byteval-value byteval) (byteval-byte byteval) 0))

(defvar *spaces* (make-hash-table :test #'equal))
(defvar *device-classes* (make-hash-table :test 'eq))
(defvar *register-formats* (make-hash-table :test 'eq))
(defvar *register-spaces* (make-hash-table :test 'eq))
(defvar *register-instances* (make-hash-table :test #'eq))
(defvar *register-instances-by-id* (make-hash-table :test #'eq))

(deftype device-class-umbrella () `(or device-class struct-device-class))

(define-container-hash-accessor *spaces* space :if-exists :continue)
(define-container-hash-accessor *device-classes* device-class :type device-class-umbrella :iterator do-device-classes)
(define-container-hash-accessor *register-formats* format :type format)
(define-container-hash-accessor *register-spaces* register-space :type space :if-exists :error)
(define-container-hash-accessor *register-instances* register-instance :type register-instance :if-exists :error)
(define-container-hash-accessor *register-instances-by-id* register-instance-by-id :type register-instance :if-exists :error)
(define-container-hash-accessor :i device :container-transform devices :parametrize-container t)
(define-container-hash-accessor :i layout :container-transform layouts :parametrize-container t)
(define-container-hash-accessor :i bitfield :container-transform bitfields :parametrize-container t :if-exists :error)
(define-container-hash-accessor :i bitfield-byte :container-transform bitfield-bytes :parametrize-container t :type cons)
(define-container-hash-accessor :i byteval :container-transform bitfield-bytevals :parametrize-container t)

;; This one stands out: going through dictionaries.
(declaim (ftype (function (space symbol) register)))
(defun register (space name)
  (declare (space space) (keyword name))
  (translation (register-dictionary space) name))

(declaim (ftype (function (symbol) fixnum) register-id))
(defun register-id (name)
  (declare (keyword name))
  (symbol-id (register-dictionary (register-space name)) name))

(declaim (ftype (function (space fixnum) register) register-by-id))
(defun register-by-id (space id)
  (declare (space space) (fixnum id))
  (id-value (register-dictionary space) id))

(declaim (ftype (function (symbol symbol) fixnum) register-selector))
(defun register-selector (register-name layout-name)
  (declare (symbol register-name layout-name))
  (let* ((space (register-space register-name))
         (layout (layout space layout-name)))
    (nth (position register-name (layout-registers layout) :key #'name) (layout-register-selectors layout))))

(defparameter *dummy-fixnum-vector* (make-array 0 :element-type 'fixnum))
(defparameter *dummy-function-vector* (make-array 0 :element-type 'function))
(defparameter *dummy-space* (make-instance 'space :name :dummy :documentation "Dummy space."))

(defstruct (struct-device-class (:include docunamed))
  (selectors *dummy-fixnum-vector* :type (vector fixnum))
  (readers *dummy-function-vector* :type (vector function))
  (writers *dummy-function-vector* :type (vector function))
  (space *dummy-space* :type space)
  (instances nil :type list)
  (layouts nil :type list)
  (effective-layout-specs nil :type list)
  (constructor nil :type (function (*) struct-device-class)))

(defparameter *dummy-struct-device-class* (make-struct-device-class :constructor #'make-struct-device-class))

(defclass device-class (standard-class)
  ((selectors :accessor device-class-selectors :type (vector fixnum) :documentation "ID-indexed register selector lookup table.")
   (readers :accessor device-class-readers :type (vector function) :documentation "ID-indexed register reader lookup table.")
   (writers :accessor device-class-writers :type (vector function) :documentation "ID-indexed register writer lookup table.")
   (space :accessor device-class-space :type (or space null))
   (layouts :accessor device-class-layouts :type list)
   (direct-layout-specs :accessor device-class-direct-layout-specs :type list :initform nil :initarg :layouts :documentation "Original layout->accessors alist.")
   (effective-layout-specs :accessor device-class-effective-layout-specs :type list :documentation "Effective layout->accessors alist.")))

(defmethod device-class-space ((o struct-device-class)) (struct-device-class-space o))
(defmethod device-class-instances ((o struct-device-class)) (struct-device-class-instances o))
(defmethod device-class-layouts ((o struct-device-class)) (struct-device-class-layouts o)) ;; for COMPUTE-INHERITED-LAYOUTS and C-D-R-I
(defmethod device-class-effective-layout-specs ((o struct-device-class)) (struct-device-class-effective-layout-specs o)) ;; for COMPUTE-INHERITED-LAYOUTS and C-D-R-I

(defclass extended-register-device-class (device-class)
  ((extensions :accessor device-class-extensions :type (vector vector) :documentation "Selector-indexed storage for extended register information.")))

(defun invalid-register-access-trap (&rest rest)
  (declare (ignore rest))
  (error "~@<Invalid register access.~:@>"))

(defmethod device-class-register-selector ((o device-class) (i #+sbcl fixnum #-sbcl integer)) (aref (device-class-selectors o) i))
(defmethod device-class-reader ((o device-class) (i #+sbcl fixnum #-sbcl integer)) (aref (device-class-readers o) i))
(defmethod device-class-writer ((o device-class) (i #+sbcl fixnum #-sbcl integer)) (aref (device-class-writers o) i))
(defmethod set-device-class-reader ((o device-class) (i #+sbcl fixnum #-sbcl integer) (fn function)) (setf (aref (device-class-readers o) i) fn))
(defmethod set-device-class-writer ((o device-class) (i #+sbcl fixnum #-sbcl integer) (fn function)) (setf (aref (device-class-writers o) i) fn))
(defmethod set-device-class-reader ((o device-class) (i #+sbcl fixnum #-sbcl integer) (fn null)) (setf (aref (device-class-readers o) i) #'invalid-register-access-trap))
(defmethod set-device-class-writer ((o device-class) (i #+sbcl fixnum #-sbcl integer) (fn null)) (setf (aref (device-class-writers o) i) #'invalid-register-access-trap))
(defsetf device-class-reader set-device-class-reader)
(defsetf device-class-writer set-device-class-writer)

(defun device-class-p (class &aux (type (class-name class)))
  (and (subtypep type 'device) (not (member type '(device extended-register-device)))))

(defun remove-if-not-subtype-of (type types)
  (remove-if-not (rcurry #'subtypep type) types))

(defun metaclass-relevant-supers (superclasses)
  (remove-if-not-subtype-of 'device superclasses))

(defun compute-metaclass (provided-metaclass provided-superclasses)
  (let ((default-metaclass (if (first (remove-if-not-subtype-of 'extended-register-device (metaclass-relevant-supers provided-superclasses)))
                               'extended-register-device-class
                               'device-class))) ;; note how this properly defaults to 'device-class when there's no M-R-S
    (or provided-metaclass default-metaclass)))

(defun compute-superclasses (provided-metaclass provided-superclasses)
  (let ((default-superclasses (case provided-metaclass
                                (extended-register-device-class 'extended-register-device)
                                ((device-class nil) 'device))))
    (append provided-superclasses (unless (metaclass-relevant-supers provided-superclasses)
                                    (list default-superclasses)))))

(defmacro define-device-subclass (name space provided-superclasses slots &rest options)
  (let* ((provided-metaclass (second (assoc :metaclass options)))
         (metaclass (compute-metaclass provided-metaclass provided-superclasses)))
    ;; XXX: shouldn't we check for the cases when user specifies E-R-D-C and DEVICE? Would V-S catch that?
    ;; (cl:format t "D-D-C ~S: defaults: metaclass: ~S, superclass: ~S;  provided: metaclass: ~S, superclasses: ~S;  final: metaclass: ~S, superclasses: ~S~%"
    ;;            name default-metaclass default-superclass provided-metaclass provided-superclasses metaclass superclasses)
    ;; (finish-output)
    `(progn
       (defclass ,name ,(compute-superclasses provided-metaclass provided-superclasses)
         (,@slots
          (selectors :allocation :class)
          (readers :allocation :class)
          (writers :allocation :class)
          ,@(when (eq metaclass 'extended-register-device-class)
                  `((extensions :allocation :class))))
         (:metaclass ,metaclass)
         (:space . ,space)
         ,@(remove-if (lambda (x) (member x '(:metaclass :space))) options :key #'first)) ;; YYY: REMOVE-FROM-ALIST
       (eval-when (:load-toplevel :execute)
         (maybe-reinitialize-device-class (find-class ',name) ',space ',(rest (assoc :layouts options)))))))

(defmacro define-device-class (name space provided-superclasses slots &rest options)
  `(define-device-subclass ,name ,space (,@provided-superclasses)
     (,@slots
      (instances :accessor instances :type list :initarg :instances :initform nil :allocation :class))
     ,@options))

(defmethod validate-superclass ((class device-class) (superclass standard-class)) t)

(defmethod validate-superclass ((class extended-register-device-class) (superclass device-class)) t)

(defmacro define-struct-device-class (name space slots &rest options)
  `(progn
     (defstruct (,name (:include struct-device))
       (,@slots))
     (initialize-struct-device-class (make-struct-device-class :name ,name ,@(when-let ((documentation (second (assoc :documentation options))))
                                                                               `(:documentation ,documentation))
                                                               :constructor (function ,(format-symbol (symbol-package name) "MAKE-~A" name)))
                                     (space ,space) ',(rest (assoc :layouts options)))))

(defstruct struct-device
  (class *dummy-struct-device-class* :type struct-device-class)
  (id 0 :type fixnum)
  (backend nil :type (or null struct-device)))

(defclass device ()
  ((selectors :accessor device-selectors :type (vector fixnum) :allocation :class) ; copied over from class
   (readers :accessor device-readers :type (vector function) :allocation :class)                     ; ...
   (writers :accessor device-writers :type (vector function) :allocation :class)                     ; ...
   (id :accessor device-id :type (integer 0))
   (backend :accessor device-backend :type (or null device) :initarg :backend)
   (category :initarg :category) ;; this might go away
   )
  (:metaclass device-class))

(defgeneric class-of-device (device)
  (:documentation "Return the DEVICE's metaclass, or device class structure,
                   depending on its type.")
  (:method ((o struct-device)) (struct-device-class o))
  (:method ((o device)) (class-of o)))

(defmethod device-id ((o struct-device)) (struct-device-id o))
(defmethod instances ((o struct-device)) (struct-device-class-instances (struct-device-class o)))
(defgeneric (setf instances) (value device))

(defmethod print-object ((device device) stream)
  (labels ((slot (id) (if (slot-boundp device id) (slot-value device id) :unbound-slot)))
    (cl:format stream "~@<#<~;~A-~A backend: ~S~;>~:@>" (type-of device) (slot 'id) (slot 'backend))))

(defclass extended-register-device (device)
  ((extensions :accessor device-extensions :type (vector vector) :allocation :class)) ; copied over from class
  (:metaclass extended-register-device-class))

(declaim (ftype (function (device fixnum) fixnum) device-register-selector))
(defun device-register-selector (device id)
  (declare (device device) (fixnum id))
  (aref (device-selectors device) id))

(defgeneric class-reallocation-effective-requirement (class new-requirement slot)
  (:documentation "Determine, with regards to CLASS's map SLOT, 
                   whether CLASS needs a new pool, an extended pool,
                   or that its current pool allocation is enough to meet
                   NEW-REQUIREMENT for SLOT.")
  (:method ((o struct-device-class) required-length slot-name)
    (cond ((member (slot-value o slot-name) (list *dummy-fixnum-vector* *dummy-function-vector*)) :new)
          ((> required-length (length (slot-value o slot-name))) :extend)))
  (:method ((o device-class) required-length slot-name)
    (cond ((not (slot-boundp o slot-name)) :new)
          ((> required-length (length (slot-value o slot-name))) :extend))))

(defun ensure-device-class-map-storage (device-class space)
  "Ensure that map storage of DEVICE-CLASS is enough to cover all registers 
   in SPACE."
  (declare (type (or struct-device-class device-class) device-class))
  (let ((required-length (length (dictionary-id-map (register-dictionary space)))))
    (flet ((new-pool (type initial-element)
             (make-array required-length :element-type type :initial-element initial-element))
           (extend-pool (old-pool initial-element)
             (concatenate (list 'vector (array-element-type old-pool))
                          old-pool (make-list (- required-length (length old-pool)) :initial-element initial-element))))
      (iter (for (slot-name type initial) in `((selectors fixnum 0)
                                               (readers function ,#'invalid-register-access-trap)
                                               (writers function ,#'invalid-register-access-trap)))
            (setf (slot-value device-class slot-name)
                  (case (class-reallocation-effective-requirement device-class required-length slot-name)
                    (:new (new-pool type initial))
                    (:extend (extend-pool (slot-value device-class slot-name) initial))
                    (t (slot-value device-class slot-name))))))))

(defun f-2 (l x) (declare (ignore l)) x)
(defun mk-f-const-or-2 (x const) (if x (constantly const) #'f-2))
(defmacro y (lambda-list &body body)
  "Idiomatic, ignore-saving lambda macro."
  (iter (for var in lambda-list)
        (if var (collect var into binds)
            (let ((var (gensym))) (collect var into binds) (collect var into ignores)))
        (finally (return `(lambda (,@binds) (declare (ignore ,@ignores)) ,@body)))))
(defun mk-f-1 (f) (y (l nil) (funcall f l)))
(defun mk-f-cdrwalk (s &aux (r s)) (y (nil nil) (prog1 (car r) (setf r (cdr r)))))

(defun fuse-map*layout (dictionary layout map fn)
  "Fuse a LAYOUT-specified subset of MAP using FN, with LAYOUT interpreted
   in the context of DICTIONARY."
  (declare (dictionary dictionary) (simple-array map) (layout layout))
  (iter (for register in (layout-registers layout))
        (let ((id (symbol-id dictionary (name register))))
          (setf (aref map id) (funcall fn id (aref map id))))))

(defun compute-accessor-function (name)
  (if (and (not (eq name t)) name) (fdefinition name) #'invalid-register-access-trap))

(defun fuse-map (space layout-specs map fuser-maker &optional sequence)
  "Fuse MAP with FN according to LAYOUT-SPECS, interpreted in the context
   of SPACE."
  (let* ((dictionary (register-dictionary space)))
    (iter (for (layout-name reader-name writer-name) in layout-specs)
          (for value = (pop sequence))
          (let ((layout (layout space layout-name)))
            (fuse-map*layout dictionary layout map (funcall fuser-maker layout reader-name writer-name value))))))

(defun compute-inherited-layouts (direct-layout-instances eligible-parents)
  (values (set-difference (remove-duplicates (apply #'append (mapcar #'device-class-layouts eligible-parents))) 
                          direct-layout-instances)
          (set-difference (remove-duplicates (apply #'append (mapcar #'device-class-effective-layout-specs eligible-parents)) :key #'car) 
                          (mapcar (compose #'list #'name) direct-layout-instances)
                          :key #'car)))

(defun initialize-struct-device-class (struct-device-class space direct-layout-specs)
  "Initialize STRUCT-DEVICE-CLASS according to SPACE and DIRECT-LAYOUT-SPECS.

   SPACE must be an instance of type SPACE.
   LAYOUT-SPECS is interpreted as a list of three-element sublists, each one
   containing a layout name and two accessor specifications, for both the
   reader and writer to be used for accessing that layout with instances made
   using STRUCT-DEVICE-CLASS.

   Accessor specifications bear one of possible following meanings, 
   with regard to corresponding accessor pools:
      - T, the pool is manually managed,
      - NIL, the pool is disabled, initialized to functions raising an error,
      - any other symbol, or setf function designator, serving as a name of
        a function used to initialize the pool."
  (declare (type struct-device-class struct-device-class) (type space space) (type list direct-layout-specs))
  (let ((direct-layout-instances (mapcar (compose (curry #'layout space) #'first) direct-layout-specs)))
    (setf (struct-device-class-layouts struct-device-class) direct-layout-instances
          (struct-device-class-effective-layout-specs struct-device-class) direct-layout-specs
          (device-class (struct-device-class-name struct-device-class)) struct-device-class)
    ;; allocate storage
    (ensure-device-class-map-storage struct-device-class (setf (struct-device-class-space struct-device-class) space))
    ;; compute and patch selector/reader/writer maps
    (with-slots (selectors readers writers) struct-device-class
      (mapc (curry #'apply #'fuse-map space direct-layout-specs)
            `((,selectors ,(y (l nil nil nil) (mk-f-cdrwalk (layout-register-selectors l))))
              (,readers   ,(y (nil r nil nil) (mk-f-const-or-2 (not (eq r t)) (compute-accessor-function r))))
              (,writers   ,(y (nil nil w nil) (mk-f-const-or-2 (not (eq w t)) (compute-accessor-function w)))))))))

(defun initialize-device-class (device-class space direct-layout-specs)
  "Initialize DEVICE-CLASS according to SPACE and DIRECT-LAYOUT-SPECS.

   SPACE must be an instance of type SPACE.
   LAYOUT-SPECS is interpreted as a list of three-element sublists, each one
   containing a layout name and two accessor specifications, for both the
   reader and writer to be used for accessing that layout with instances made
   using DEVICE-CLASS.

   Accessor specifications bear one of possible following meanings, 
   with regard to corresponding accessor pools:
      - T, the pool is manually managed,
      - NIL, the pool is disabled, initialized to functions raising an error,
      - any other symbol, or setf function designator, serving as a name of
        a function used to initialize the pool."
  (declare (type device-class device-class) (type (or null space) space) (type list direct-layout-specs))
  (if space
      (let ((direct-layout-instances (mapcar (compose (curry #'layout space) #'first) direct-layout-specs))
            (eligible-parents (remove-if-not (lambda (pc) (and (device-class-p pc) (device-class-space pc))) (class-direct-superclasses device-class))))
        (when-let ((misspaced (remove-if (curry #'eq space) (mapcar #'device-class-space eligible-parents))))
          (error "~@<During initialization of device class ~S: cannot do cross-space inheritance: ~S vs. ~S.~:@>"
                 (class-name device-class) (space-name space) (mapcar #'space-name misspaced)))
        (multiple-value-bind (inherited-layout-instances inherited-layout-specs) (compute-inherited-layouts direct-layout-instances eligible-parents)
          ;; fill in the basics and register self
          (setf (device-class-layouts device-class) (append inherited-layout-instances direct-layout-instances)
                (device-class-effective-layout-specs device-class) (append inherited-layout-specs direct-layout-specs)
                (device-class (class-name device-class)) device-class)
          ;; allocate storage
          (ensure-device-class-map-storage device-class (setf (device-class-space device-class) space))
          ;; compute and patch selector/reader/writer maps
          (with-slots (selectors readers writers) device-class
            (let ((providing-parents (mapcar (rcurry #'find eligible-parents :key #'device-class-layouts :test #'member) inherited-layout-instances)))
              (mapc (curry #'apply #'fuse-map space direct-layout-specs)
                    `((,selectors ,(y (l nil nil nil) (mk-f-cdrwalk (layout-register-selectors l))))
                      (,readers   ,(y (nil r nil nil) (mk-f-const-or-2 (not (eq r t)) (compute-accessor-function r))))
                      (,writers   ,(y (nil nil w nil) (mk-f-const-or-2 (not (eq w t)) (compute-accessor-function w))))))
              (mapc (curry #'apply #'fuse-map space inherited-layout-specs) 
                    `((,selectors ,(y (nil nil nil p) (y (id nil) (aref (device-class-selectors p) id))) ,providing-parents)
                      (,readers   ,(y (nil nil nil p) (y (id nil) (aref (device-class-readers p) id))) ,providing-parents)
                      (,writers   ,(y (nil nil nil p) (y (id nil) (aref (device-class-writers p) id))) ,providing-parents)))))))
      (if direct-layout-specs
          (error "~@<During initialization of device class ~S: layouts can not be specified without space.~:@>" (class-name device-class))
          ;; Messing with initargs would be way too painful...
          (with-slots (space layouts direct-layout-specs effective-layout-specs selectors readers writers) device-class
            (setf (values space layouts direct-layout-specs effective-layout-specs selectors readers writers)
                  (values nil nil nil nil (make-array 0 :element-type 'fixnum)
                          (make-array 0 :element-type 'function :initial-element #'identity) (make-array 0 :element-type 'function :initial-element #'identity)))))))

(defun reinitialize-device-class (device-class &aux (device-class (xform-if #'symbolp #'find-class device-class)))
  "Reinitialize DEVICE-CLASS according to its SPACE and DIRECT-LAYOUT-SPECS slots."
  (initialize-device-class device-class (device-class-space device-class) (device-class-direct-layout-specs device-class)))

(defun device-class-corresponds-to-space-and-layouts-p (device-class space-name layout-specs)
  (when-let ((present-space (device-class-space device-class)))
    (and (eq present-space (space space-name))
         (equal (device-class-direct-layout-specs device-class) layout-specs)
         (every #'eq (device-class-layouts device-class) (mapcar (curry #'layout present-space) (mapcar #'car layout-specs))))))

(defun maybe-reinitialize-device-class (device-class space-name direct-layout-specs)
  "Reinitialize an already defined DEVICE-CLASS according to SPACE-NAME and 
   DIRECT-LAYOUT-SPECS, if they differ from stored values."
  (unless (device-class-corresponds-to-space-and-layouts-p device-class space-name direct-layout-specs)
    (with-slots (space (direct-layout-specs-slot direct-layout-specs)) device-class
      (setf (values space direct-layout-specs-slot) (values (space space-name) direct-layout-specs)))
    (reinitialize-device-class device-class)))

;;;
;;; XXX: not pretty: hack around non-&allow-other-keys-able initargs...
;;;
(defmethod initialize-instance ((o device-class) &rest initargs)
  (apply #'shared-initialize o t (remove-from-plist initargs :space)))
(defmethod reinitialize-instance ((o device-class) &rest initargs)
  (apply #'shared-initialize o nil (remove-from-plist initargs :space)))

(defmethod initialize-instance :after ((o device-class) &key space layouts &allow-other-keys)
  (initialize-device-class o (when space (space space)) layouts))

(defun build-device-class-extension-map (space layout-names)
  (lret* ((dictionary (register-dictionary space))
          (length (length (dictionary-id-map dictionary)))
          (extension-map (make-array length :element-type 'vector :initial-element #())))
    (let ((candidate-extensions (iter outer
                                      (for layout-name in layout-names)
                                      (for layout = (layout space layout-name))
                                      (iter (for register in (layout-registers layout))
                                            (for selector in (layout-register-selectors layout))
                                            (in outer (collect (cons selector (map 'vector #'identity (cons register (reg-ext register))))))))))
      (unless (= (length (remove-duplicates candidate-extensions :key #'car))
                 (length candidate-extensions))
        (error "~@<Cannot build a register extension map for intersecting layouts: ~S~:@>" layout-names))
      (iter (for (selector . extension) in candidate-extensions)
            (setf (aref extension-map selector) extension)))))

;;;
;;; XXX: not pretty: hack around non-&allow-other-keys-able initargs...
;;;
(defmethod initialize-instance ((o extended-register-device-class) &rest initargs)
  (apply #'shared-initialize o t (remove-from-plist initargs :extended-layouts)))
(defmethod reinitialize-instance ((o extended-register-device-class) &rest initargs)
  (apply #'shared-initialize o nil (remove-from-plist initargs :extended-layouts)))

(defmethod initialize-instance :after ((o extended-register-device-class) &key extended-layouts &allow-other-keys)
  (when (device-class-p o)
    (when-let ((missing (remove-if (rcurry #'assoc (device-class-effective-layout-specs o)) extended-layouts)))
      (error "~@<During initialization of extended register device class ~S: unknown layouts were specified to be extended: ~S~:@>"
             (class-name o) missing))
    ;; XXX: no inheritance for these maps... complicated composition. Not unsurmountable, though.
    (setf (device-class-extensions o) (build-device-class-extension-map (device-class-space o) extended-layouts))))

(defun device-type (device)
  "Return the DEVICE's category, which is supposed to be a \"more
   general type\", independent of flavor variations.

   Used in:
      - runtime device nomenclature (target-device, device-hash-id),
      - device type lookups."
  (slot-value* device 'category (type-of device)))

;;;;
;;;; The decisive question is what factors into the register instance name.
;;;;
(defun device-hash-id (device)
  (list (device-type device) (device-id device)))

(defun create-device-register-instances (device &aux (device-class (class-of-device device)))
  "Walk the DEVICE's layouts and spawn the broodlings."
  (labels ((name-to-reginstance-name (name layout device)
	     (format-symbol :keyword (layout-name-format layout) name (1- (device-id device)))))
    (iter (for layout in (device-class-layouts device-class))
          (for (nil reader-name writer-name) in (device-class-effective-layout-specs device-class))
          (iter (for register in (layout-registers layout))
                (for selector in (layout-register-selectors layout))
                (let* ((name (name-to-reginstance-name (name register) layout device))
                       (id (1+ (hash-table-count *register-instances-by-id*)))
                       (instance (make-register-instance :name name :register register :device device :selector selector :id id
                                                         :reader (compute-accessor-function reader-name) :writer (compute-accessor-function writer-name))))
                  (setf (register-instance-by-id id) instance)
                  (iter (for riname in (cons name (mapcar (rcurry #'name-to-reginstance-name layout device)
                                                          (reg-aliases register))))
                        (assert riname)
                        (setf (register-instance riname) instance)))))))

(defmethod initialize-instance :around ((device device) &key &allow-other-keys)
  (when *verbose-device-init-p*
    (cl:format *log-stream* "~S " (type-of device))
    (finish-output *log-stream*))
  (call-next-method))

(defun make-struct-device-instance (type &rest initargs)
  (lret* ((class (device-class type))
	  (space (struct-device-class-space class))
          (instance (apply (struct-device-class-constructor class) :id (1- (length (struct-device-class-instances class))) initargs)))
    (push instance (struct-device-class-instances class))
    (setf (gethash (device-hash-id instance) (devices space)) instance)
    (create-device-register-instances instance)))

(defmethod initialize-instance :after ((device device) &key &allow-other-keys)
  (let* ((device-class (class-of device))
         (space (device-class-space device-class)))
    (unless space
      (error "~@<During device initialization: type ~S claims no space.~:@>" (type-of device)))
    (push device (instances device))
    (setf (device-id device) (1- (length (instances device)))
          (device-selectors device) (device-class-selectors device-class)
          (device-readers device) (device-class-readers device-class)
          (device-writers device) (device-class-writers device-class)
          ;; register within space
          (gethash (device-hash-id device) (devices space)) device)
    ;; (cl:format t "Initialized ~S/~S: ~S, ~S, ~S~%" (class-name (class-of device)) (device-id device) (device-selectors device) (device-readers device) (device-writers device))
    (create-device-register-instances device)))

(defmethod initialize-instance :after ((device extended-register-device) &key &allow-other-keys)
  (setf (device-extensions device) (device-class-extensions (class-of device))))

(defun space-device-count (space)
  (hash-table-count (devices space)))

(defun space-remove-device (device)
  (let* ((device-class (class-of device))
         (space (device-class-space device-class)))
    (remhash (device-hash-id device) (devices space))
    (removef (instances device) device)))

(defun init-device-model ()
  "Forget all known device and register instances."
  (iter (for (nil space) in-hashtable *spaces*)
        (when (atom (space-name space))
          (clrhash (devices space))))
  (do-device-classes (device-class)
    (unless (class-finalized-p device-class)
      (finalize-inheritance device-class))
    (for prototype = (class-prototype device-class))
    (when (slot-exists-p prototype 'instances)
      (setf (slot-value prototype 'instances) nil)))
  (clrhash *register-instances*)
  (clrhash *register-instances-by-id*))

(define-condition bit-notation-condition (error) ())

(define-reported-condition bit-notation-no-space-context-error (bit-notation-condition)
  ()
  (:report () "~@<Attempt to use names in a null space context.~:@>"))

(define-reported-condition bit-notation-conflicting-bitfield-names (bit-notation-condition)
  ((conflicting-bitfield :initarg :conflicting-bitfield)
   (expected-format :initarg :expected-format))
  (:report (conflicting-bitfield expected-format)
           "~@<Encountered unexpected reference to bitfield ~S within context of register format ~S.~:@>" conflicting-bitfield expected-format))

(defun bitfield-format (space bitfield-name)
  "Yield the format of BITFIELD-NAMEd in SPACE"
  (bitfield-format% (bitfield space bitfield-name)))

(defun register-format-field-type (name)
  (format-symbol t "~A-BITFIELD" name))

(defun define-bitfield (format name size pos doc &optional bytevals)
  (let* ((byte (byte size pos))
	 (space (format-space format))
	 (bitfield (make-bitfield :format% format :name name :spec byte :documentation doc)))
    (push bitfield (format-bitfields format))
    (loop :for (value name documentation) :in bytevals
                                          :do (let ((byteval (make-byteval :name name :documentation documentation :byte byte :value value)))
                                                (setf (gethash name (bitfield-bytevals bitfield)) byteval
                                                      (gethash value (bitfield-byterevvals bitfield)) byteval)))
    (dolist (space (list* space (mapcar #'space (space-referrers space))))
      (setf (gethash name (bitfield-bytes space)) byte
	    (gethash name (bitfields space)) bitfield))))

;; an ability to pluck in a QUOTE would've been very nice...
(defun define-register-format-notype (space name documentation bitspecs)
  (let ((format (make-format :name name :documentation documentation :space space)))
    (mapc [apply [define-bitfield format]] bitspecs)
    (setf (format name) format)))

(defmacro define-register-format (&environment env name doc &rest bitspecs)
  `(eval-when (:compile-toplevel :load-toplevel)
     (deftype ,(register-format-field-type name) () `(member ,,@(mapcar #'car bitspecs)))
     (define-register-format-notype (space ,(space-name (space (environment-space-name-context env)))) ',name ,doc ',bitspecs)))

(defun define-register (layout name &key (doc "Undocumented register.") aliases format ext)
  (lret* ((space (layout-space layout))
          (register (make-register :layout layout :name name :space space :documentation doc
                                   :format (when format (format format)) :ext ext
                                   :aliases aliases
                                   :type (register-format-field-type format))))
    ;; YYY: this is a kludge: a proper EVAL-WHEN somewhere is direly needed...
    (unless (register-space name :if-does-not-exist :continue)
      (setf (register-space name) space))
    (add-symbol (register-dictionary space) name register nil)))

(defun ensure-layout (space name documentation name-format register-specs &aux (selectors (mapcar #'second register-specs)))
  (unless (every (of-type 'fixnum) selectors)
    (error "~@<While defining layout ~S: register selectors must be of type FIXNUM.~:@>" name))
  (lret ((layout (make-layout :name name :space space :documentation documentation
                              :register-selectors selectors :name-format name-format)))
    (setf (gethash name (layouts space)) layout
          (layout-registers layout) (iter (for (name selector . rest) in register-specs)
                                          (collect (apply #'define-register layout name rest))))))

(defmacro define-layout (&environment env (name doc &key name-format) &rest defs)
  `(ensure-layout (space ,(space-name (space (environment-space-name-context env)))) ',name ,doc ,name-format ',defs))

;;;
;;;  o  layout templates
;;;  o  register instantiation
;;;;    - what information is deduced from register instances /now/?
;;;;      ... survey
;;;
(defmacro define-namespace (name &body f)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(once-only (name)
        `(setf (space ,name) (make-instance 'space :name ,name :documentation ,(cadr (assoc :documentation f)))))
     (symbol-macrolet ((*space* ,name))
       ,@(mapcar [cons 'define-register-format] (cdr (assoc :register-formats f)))
       ,@(mapcar [cons 'define-layout] (cdr (assoc :layouts f))))))

(defun undefine-space (name)
  (remhash name *spaces*))
  
(defun unify-namespaces (names)
  (let* ((spaces (mapcar #'space names))
	 (unispace (make-instance 'space :name names
					 :documentation (cl:format nil "Unified namespace of:~{ ~S~}."
                                                                   (mapcar #'space-documentation spaces)))))
    (labels ((check-import-unispace (accessor-name key val)
	       (let* ((accessor-fn (fdefinition accessor-name))
		      (hash-table (funcall accessor-fn unispace)))
		 (when (gethash key hash-table)
		   (error "Namespace conflict while trying to unify namespaces ~S: accessor ~S, key ~S." names accessor-name key))
		 (setf (gethash key hash-table) val))))
      (dolist (space spaces)
	(pushnew names (space-referrers space) :test #'equal)
	(dolist (accessor-name '(bitfields bitfield-bytes layouts))
	  (let ((checker-importer (curry #'check-import-unispace accessor-name)))
	    (maphash checker-importer (funcall (fdefinition accessor-name) space))))))
    (setf (space names) unispace)))

(defun environment-space-name-context (env)
  (let ((space-name (macroexpand-1 '*space* env)))
    (unless space-name
      (error 'bit-notation-no-space-context-error))
    space-name))

(defmacro space-name-context (&environment env)
  (list 'quote (environment-space-name-context env)))

(defmacro with-namespaces ((&rest nsnames) &body body)
  (let* ((need-unification (> (length nsnames) 1))
         (name (if need-unification nsnames (first nsnames))))
    (when-let ((orphan (find-if-not #'space nsnames)))
      (error "reference to an undefined namespace ~S" orphan))
    `(progn
       ,@(when need-unification
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (unify-namespaces ',nsnames))))
       (symbol-macrolet ((*space* ,name))
         ,@body))))

(defmacro set-namespace (&rest nsnames)
  (let* ((need-unification (> (length nsnames) 1))
	 (name (if need-unification nsnames (first nsnames))))
    (when-let ((orphan (find-if-not #'space nsnames)))
      (error "reference to an undefined namespace ~S" orphan))
    `(eval-when (:compile-toplevel :load-toplevel)
       ,@(when need-unification
	       `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (unify-namespaces ',nsnames))))
       (define-symbol-macro *space* ,name))))

(defun bitfield-decode (bitfield value &key (symbolise-unknowns t))
  (declare (type bitfield bitfield) (type (unsigned-byte 32) value))
  (cond ((plusp (hash-table-count (bitfield-bytevals bitfield))) ;; bitfield-enumerated-p?
	 (let* ((val (ldb (bitfield-spec bitfield) value)))
	   (if-let ((field (gethash val (bitfield-byterevvals bitfield))))
		   (name field)
                   (if symbolise-unknowns
                       (format-symbol :keyword "UNKNOWN-VALUE-~B" val)
                       val))))
	((> (car (bitfield-spec bitfield)) 1) ;; bitfield-wide-p?
	 (ldb (bitfield-spec bitfield) value))
	(t
	 (ldb-test (bitfield-spec bitfield) value))))

(defun format-decode (format value &key (symbolise-unknowns t))
  (declare (type format format) (type (unsigned-byte 32) value))
  (iter (for bitfield in (format-bitfields format))
        (collect (cons (name bitfield) (bitfield-decode bitfield value :symbolise-unknowns symbolise-unknowns)))))

;;;;
;;;;
;;;;  F R O N T E N D
;;;;
;;;;
(defun bytenames-ensure-same-register (space regname bytenames)
  "Deduce register difference from register format difference and signal an error, if any."
  (let ((fmt (if regname (reg-format (register space regname))
                 (bitfield-format space (first bytenames)))))
    (if-let ((stray (find-if-not {[eq fmt] [bitfield-format space]} bytenames)))
	    (error 'bit-notation-conflicting-bitfield-names
		   :expected-format (name fmt) ;; (name reg) (name (bitfield-format space stray))
                   :conflicting-bitfield stray))))

(defun mkenv (space-name bitfield-name &aux (bitfield (bitfield (space space-name) bitfield-name)))
  (make-environment
   :byte (bitfield-spec bitfield)
   :bindings
   (if (or (null space-name) (null bitfield-name))
       (make-hash-table)
       (xform-hash-table #'flatten-byteval (bitfield-bytevals bitfield)))))

(defun find-space-with-bytenames (bytenames)
  "Find the first non-compound space containing every bitfield in BYTENAMES."
  (iter (for (name space) in-hashtable *spaces*)
        (when (and (symbolp name)
                   (every (lambda (bf) (bitfield space bf :if-does-not-exist :continue)) bytenames))
          (return space))))

(defmacro decode-context ((spacename &optional space-want bitfield fmtname) regname bytenames &body body)
  (unless (or regname bytenames)
    (error "~@<Impossible to deduce context: neither register name, nor byte names were specified.~:@>"))
  (let ((space (or space-want (gensym))) (newbytenames (gensym)))
    `(let* ((,space ,(if regname `(register-space ,regname) `(find-space-with-bytenames (ensure-list ,bytenames)))) (,spacename (space-name ,space))
            ,@(when bytenames `((,newbytenames (ensure-list ,bytenames))))
            ,@(when (and bitfield bytenames) `((,bitfield (bitfield ,space (car ,newbytenames)))))
            ,@(when fmtname `((,fmtname (and ,regname (xform-if #'identity #'name (reg-format (register ,space ,regname))))))))
       (declare (ignorable ,spacename ,space
                           ,@(when bytenames `(,newbytenames))
                           ,@(when (and bitfield bytenames) `(,bitfield))
                           ,@(when fmtname `(,fmtname))))
       ,@(when bytenames `((bytenames-ensure-same-register ,space ,regname ,newbytenames)))
       ,@body)))

;;;
;;; So, there are two register access schemes.
;;; First way is to access registers via combination of device information
;;; and abstract register name. This is good for debugger-internal code.
;;; The second one derives all information from unique, device-specific
;;; register instances. This is better suited for interactive use.
;;;
(defun device-register (device register-id)
  (declare (type device device) (type fixnum register-id))
  (funcall (aref (device-readers device) register-id) device (aref (device-selectors device) register-id)))

(defun set-device-register (device register-id value)
  (declare (type device device) (type (unsigned-byte 32) value))
  (funcall (aref (device-writers device) register-id) value device (aref (device-selectors device) register-id)))

(defun reginstance-value (register-instance)
  (declare (type register-instance register-instance))
  (let ((device (reginstance-device register-instance)))
   (funcall (reginstance-reader register-instance) device (reginstance-selector register-instance))))

(defun set-reginstance-value (register-instance value)
  (declare (type register-instance register-instance))
  (let ((device (reginstance-device register-instance)))
    (funcall (reginstance-writer register-instance) value device (reginstance-selector register-instance))))

(defsetf device-register set-device-register)
(defsetf reginstance-value set-reginstance-value)
    
(defmacro devreg (device regname)
  `(device-register ,device (load-time-value (register-id ,regname))))

(define-setc-expander devreg (value device regname)
  `(set-device-register ,device (load-time-value (register-id ,regname)) ,(eeval value)))

(defmacro decode (fmtname value &key (symbolise-unknowns t))
  (if (constant-p fmtname) ;; we won't do the same for other obvious cases
      `(format-decode (load-time-value (format ,fmtname)) ,value :symbolise-unknowns ,symbolise-unknowns)
      `(format-decode (format ,fmtname) ,value :symbolise-unknowns ,symbolise-unknowns)))

(defmacro devbit-decode (device regname bytename)
  (decode-context (space-name space bitfield) regname `(,bytename)
    `(bitfield-decode (load-time-value (bitfield (space ',space-name) ,bytename))
		      (device-register ,device (load-time-value (register-id ,regname))))))

(defmacro devreg-decode (device regname)
  (decode-context (space-name space bitfield fmtname) regname ()
    `(format-decode (load-time-value (format ,fmtname))
		    (device-register ,device (load-time-value (register-id ,regname))))))

(defmacro devbit (device regname bytename)
  (decode-context (space-name space) regname `(,bytename)
    `(ldb-test ',(bitfield-byte space bytename)
	       (device-register ,device (load-time-value (register-id ,regname))))))
  
(defmacro devbit-value (device regname bytename)
  (decode-context (space-name space) regname `(,bytename)
    `(ldb ',(bitfield-byte space bytename)
	  (device-register ,device (load-time-value (register-id ,regname))))))

(define-setc-expander devbit (value device regname bytename &key write-only)
  (decode-context (space-name space bitfield) regname `(,bytename)
    (let ((mask (byte-bitmask (bitfield-spec bitfield))))
      `(setf (device-register ,device (load-time-value (register-id ,regname)))
	     ,(eeval
	       `(logior ,value ,(unless write-only
                                  `(device-register ,device (register-id ,regname))))
	       `(,mask ,(lognot mask))
	       `(,(mkenv space-name (name bitfield)) nil))))))

(defmacro devbits (device regname (&rest bytenames))
  (decode-context (space-name space bitfield) regname bytenames
    `(values-list 
      (mapcar
       (rcurry #'ldb-test (device-register ,device (load-time-value (register-id ,regname))))
       ',(mapcar [bitfield-byte space] bytenames)))))

(define-setc-expander devbits (values device regname (&rest bytenames) &key write-only)
  (decode-context (space-name space bitfield) regname bytenames
    (with-gensyms (device-var reg-id-var) 
      (let* ((initial (unless write-only
                        `(device-register ,device-var ,reg-id-var)))
	     (bytes (mapcar [bitfield-byte space] bytenames)))
	`(let ((,device-var ,device)
	       (,reg-id-var (load-time-value (register-id ,regname))))
	   (setf (device-register ,device-var ,reg-id-var)
		 ,(eeval (list* 'logior initial (ensure-destructurisation bytenames values))
			 (list* (lognot (bytes-bitmask bytes)) (mapcar #'byte-bitmask bytes))
			 (list* nil (mapcar [mkenv space-name] bytenames)))))))))

(defmacro bits (bytenames &rest bytevals)
  "Combined bytevals of bitfields specified by BYTENAMES/BYTEVALS.
  Bytevals default to T, when left completely unspecified."
  (decode-context (space-name space) nil bytenames
    (let* ((bytenames (ensure-list bytenames))
	   (bytevals (or bytevals (make-list (length bytenames) :initial-element t))))
      (eeval (list* 'logior bytevals)
	     (mapcar {#'byte-bitmask [bitfield-byte space]} bytenames)
	     (mapcar [mkenv space-name] bytenames)))))

(defmacro test-devbits (device regname bytenames &rest bytevals)
  "Check if every bitfield among those specified by BYTENAMES is set to a value denoted by
   a corresponding member of BYTEVALS."
  (decode-context (space-name space bitfield) nil bytenames
    (let ((bytenames (ensure-list bytenames)))
      `(= (logand (device-register ,device (load-time-value (register-id ,regname)))
		  ,(bytes-bitmask (mapcar [bitfield-byte space] bytenames)))
	  (bits ,bytenames ,@bytevals)))))

(defmacro test-bits (&environment env (&rest bytenames) val)
  "Check if every bitfield specified by BYTENAMES is set to all 1's."
  (let ((mask (bytes-bitmask (mapcar [bitfield-byte (space (environment-space-name-context env))] bytenames))))
    `(= (logand ,val ,mask) ,mask)))

(defmacro bit-value (value bytename)
  (decode-context (space-name space) nil `(,bytename)
    `(ldb ',(bitfield-byte space bytename) ,value)))
